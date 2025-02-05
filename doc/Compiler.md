# A tour of the mirac compiler internals
The mirac compiler comprises a hierarchy of 26 modules, each performing a specific function in the compiler pass pipeline
from source file to asm binary.  We'll tour these in a bottom-up order, based upon when they are used in the pipeline.

The overall pipeline looks like:

`source -> tokenize -> parse -> AST transformation passes -> STG -> codegen`

A module's text is tokenized, then parsed into an Abstract Syntax Tree (AST).  From there it is
run through a sequence of AST transformation passes.  All of the collected module ASTs are combined
and lowered to a Spineless, Tagless G-machine representation (STG), which is then converted to
x86-64 assembly code.

### AST transformation passes (as defined in `mirac.m`):

    [ reifyImports
    , reifyDefns isDefData        || reify data type and constructor names early for desugaring constructor patterns
    , deriveInstances             || derive any missing instances for show, ord from the DefData and DefSyn defns
    , desugar                     || desugar complex pats, list comprehensions, conditionals, etc.
    , reifyDefns notData          || reify all the desugared fn and pat definitions
    , rename                      || qualify or uniquely rename as Internal all Unqualified names
    , analyze                     || free var analysis for rewrite
    , rewrite                     || organize Elets for typecheck, cleanup and general optimizations   
    , analyzeWarn                 || re-do free var analysis after rewrite and check for unused variables and non-exhaustive patterns
    , reifySyns                   || expand type synonyms to base types, checking for cycles
    , reifyTypes                  || expand specs and dataDefs with expanded type synonyms
    , typecheck                   || type check the module
    , streamSubst                 || do stream-based substitution of standard list-based functions
    , inline                      || do inlining of small, non-recursive functions, ast simplification
    , normalize                   || rewrite to A-Normal Form and saturate calls to known functions
    , demand                      || demand analysis to discover strictness and rewrite strict-use thunks to Ecase exprs
    , analyze                     || final free var analysis for STG (normalization may have created new thunks)
    , reifyDefAbs                 || hide abstype implementations
    , reifyExports                || process export libParts and create export nameMap
    ]

## `name.m`
We'll start with the module `name.m`, which defines the `name` data type used throughout the compiler to specify
variable, function, constructor, and type names.  `name`s can be one of:

* `Unqualified`: a name that has not yet been qualified with a module
* `Unresolved`: a name that is qualified by a module in the source code, but hasn't been resolved (verified)
* `Qualified`: a resolved name qualified with its associated module
* `Private`: like `Qualified`, but cannot be exported / imported
* `Builtin`: a resolved name for a builtin function or type
* `Internal`: a resolved, non-top-level name which is qualified with a unique integer

The module also defines a number of common functions on `name`s.

## `grammar.m`
The `grammar` module defines the AST for Miranda2 that will be used and refined in all the subsequent passes.
It firsts defines a number of type aliases for `ast`:

* `expr`: a value expression
* `cond`: conditional expressions (ones qualified by `if` or `otherwise`)
* `caseAlt`: a case expression alternate
* `qual`: a list comprehension generator, recurrence, or guard
* `pat`: a pattern
* `texpr`: a type expression
* `tform`: the left hand side of a type definition
* `defn`: a definition

These aliases all are equivalent to the unified `ast` data type, but are useful for documenting which part of the ast we are
focused on.  The `ast` itself is defined next, along with auxiliary types `caseSel` and `multSel`.  Finally, the `anno` type
alias is defined as a tuple of annotation information that is attached to every `defn`:

* location: a tuple of module name, line number, and column number
* recursive: a bool indicating whether the definition is recursive / mutually-recursive or not
* complexity: an int indicating the rough size (number of AST nodes) of the `defn`, used to inform inlining
* use count: an count of the number of distinct uses of this `defn`, used in inlining, unused warning reporting, and dead-code elimination
* free vars: a set of names that are used as variables in the `defn`, but not defined in the `defn`'s scope
* strictness: an int (bit set) with the individual bits specifying whether the corresponding argument index is strict or not

During the initial design, it was thought that the anno info would only need to be attached to a `defn`, but general error
reporting probably needs to be more fine-grained than that.  Also, it would be nice to annotate every AST node with its
inferred type after typechecking.  So in the future, the annotation field may be expanded and applied to other ast nodes, or
replaced with something else.

After this, `grammar.m` goes on to define `showS`, a faster way of appending strings by composing functions, then uses it
to define various functions to pretty-print an `ast`.  Then come common operations on `expr`s, `pat`s, `texpr`s, and `defn`s,
Then we define two important map data types that are used frequently throughout the compiler:

* `nameMap`: a map from an Unqualified or Unresolved name to a Qualified Name
* `defnMap`: a map from a Qualified name to a `defn`

Finally, we define a fixed precedence and associativity table for common operators.  This will likely be replaced with a
user-definable mechanism later on.

## `ast.m`
The `ast` module (which should probably be named "traversal") defines functions for traversing an `ast`, including
accumulations of information and rewrites of the `ast`.

### Accumulation traversals
An `ast` traversal can be used to collect information from an `ast`.  Accumulation traversals for bottom-up, top-down, and
top-down with continuation are defined.  The top-down with continuation traversal passes the explicit continuation of
traversing the sub-ASTs of an AST node into the astAccum function, to allow it to do things like short-circuit the traversal
or substitute a custom traversal for the sub-ASTs. Also, an accumulation traversal called `astAccumLex` is defined, in which the
accumulator state passed through the traversal is split into a dynamically-scoped component and a lexically-scoped component.
This allows top-down lexical information (such as location) to be attached to the traversal and automatically removed when the
traversal returns.

### Rewriting traversals
An `ast` traversal can also be used to rewrite an `ast`, passing state information along during the traversal.   The state is
handled with a `state` monad (defined in `lib/state.m`).  Rewriting traversals for bottom-up, top-down, top-down with continuation
are defined, and, analogous to `astAccumLex`, an `astRewriteLex` implements top-down rewriting with additional lexical scope state
attached.

## `tokenizer.m`
The `tokenizer` module implements functions to convert a character stream (lazy list of characters) to a `tokloc` stream (lazy list
of tokens along with their source location), by removing whitespace and grouping characters to form a token. The main function is
`tokenizePos`, which takes the current line and column numbers and calls specialized tokenizers based upon the next 1 or 2
characters of the input string.  Each of the specialized tokenizers is written in a Continuation Passing Style (CPS) to allow a
called tokenizer to "return" multiple values (e.g. updated row, col, and character stream) to a continuation, without having to
use a tuple to structure/destructure the values.  This resulted in a measureable performance improvement, because the tokenizer is
called for each character in the input string.

### Qualified Identifiers and Symbols
The tokenizer processes identifiers by first checking to see if the next character immediately following the ident is a `.`. If
so, the identifer is treated as the module name of an Unresolved identifier or symbol, which is tokenized from the characters
following the `.`.  If not, the identifier is returned as an Unqualified identifier.

### Handling `$`
The tokenizer handles `$` in two different ways: if the `$` is immediately followed (with no whitespace) by a letter or `_`, it
signifies an identifer that is to be used as an infix operator, e.g. `$div` or `$parser.p_satisfy`, and returns that identifier as
a Tsymbol.  Otherwise it is treated as a standalone symbol (defined in `stdlib.m` as `apply`) or part of another symbol.

### Handling a `#` suffix
`tokenizeStr`, `tokenizeLitInt`, and `tokenizeLitChar` all check to see if the next character after tokenization is a `#`.  If so,
`tokenizeStr` appends the `#` as part of the name (to be processed as a normal identifier, but with the explicit `#` suffix).
`tokenizeLitInt` and `tokenizeLitChar` use it to return a different token type (`TprimInt` and `TprimChar`, respectively), to be
used later as unboxed primitive values.

### Expanding escape characters in strings
Literal strings are tokenized in `tokenizeString` by repeatedly calling `tokenizeChar`, which handles the correct tokenization of
escaped characters like `\n` or `\x32`.

## `parser.m`
Parsing is split into two separate modules `parser` and `mirandaParser`.  `parser` implements the basic parsing combinators
for processing a `tokloc` stream, while `mirandaParser` implements parsing combinators specific to the Miranda2 grammar.  This
split allows `parser` to be used in the `predef` module as well (to parse builtin type specifications); otherwise a dependency
loop between the `mirandaParser` and `predef` modules would exist.

### Parser state
The parser is based on `lib/maybeState`, which implements a state monad augmented by a maybe monad.  This allows a parser to
return a (`Just` value) upon success, or a `Nothing` upon failure, with `p_bind` (and its associated applicative functors)
automatically short-circuiting the return of any failure in a chain of parser combinators.

The state passed through the parsers, `psSt`, is a tuple of
* the module name
* the deepest error encountered during parsing, so far
* a stack of active indent levels to handle offside-rule processing
* the (lazy) list of tokloc values from the tokenizer

Most of the parser combinators are written in a curried form, not mentioning the parser as an operand.
Instead, state is handled implicitly by lower-level combinators (e.g. `p_any`), or by the combinator p_get, which returns
the current state.  This, combined with the automatic early-out handling of parsing failures results in high-level parser
combinators which are easier to read. For example, the parser `p_inParens`:

    p_inParens :: parser * -> parser *
    p_inParens p = p_char '(' >> p << p_char ')'

parses the parser `p` surrounded by parenthesis tokens, without having to show the automatic handling of conditions
like:
* end-of-file encountered
* offside-rule indentation error
* token match failure on any of the parsers

### Error handling
The error portion of the parser state `psErr` is a tuple of
* error string
* severity
* line number
* column number

#### `p_error`
When the parser `p_error` is called with a severity value and error string, it compares the error against the `psErr`
held in the parser state, and replaces the `psErr` value with whichever compares `max` (the deepest via line/col
number, or, if equal, the severity).  This is a heuristic to pick which error to report when all parsing alternatives
have failed, the theory being that the best error to report is likely to be the one that proceeded the furthest
before encountering an error.

#### `p_alt`
The alternative parser, `p_alt` tries to run the primary parser, and, if that fails, runs the alternative parser
with the original parser state ("rewinding" the tokloc stream), except it will update the `psErr` part of the
state with that of the first parser.

#### `p_any`
The parser `p_any` is the most basic token parser.  It is the one that explicitly handles most of the fundamental
parsing errors (end-of-input, error from tokenizer, and the offside-rule error.  Most other parser combinators
use it indirectly.

#### `p_guard` and `p_satisfy`
The parser combinator `p_guard` takes a primary parser and a predicate parser, and passes the results of running
the primary parser to the predicate parser.  If the predicate parser (the "guard" test) fails, then the
`p_unexpected` parser is run, which fails and runs p_error with the message "unexpected" and the current token.
The parser combinator `p_satisfy` takes a parser and a predicate function, and passes the result of the parser to a
`p_guard` parser, which tests the result with the predicate function, and if it is false, fails, invoking the
`p_unexpected` alternate of the `p_guard`.

Most other token parsers are written to use a combination of `p_any` and `p_satisfy`, for example `p_token`, a
parser which expects a particular literal token value:

    p_token :: token -> parser token
    p_token t = p_any $p_satisfy (_eq cmptoken t)


## `mirandaParser.m`

## `exception.m`

## `config.m`

## `predef.m`

## `dependency.m`

## `module.m`

## `reify.m`

## `derive.m`

## `desugar.m`

## `rename.m`

## `analyze.m`

## `rewrite.m`

## `typecheck.m`

## `inline.m`

## `normalize.m`

## `demand.m`

## `serialize.m`

## `stg.m`

## `codegen.m`

## `mirac.m`
