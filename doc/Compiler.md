# A tour of the mirac compiler internals
The mirac compiler comprises a hierarchy of 26 modules, each performing a specific function in the compiler pass pipeline
from source file to asm binary. We'll tour these in a bottom-up order, based upon when they are used in the pipeline.

The overall pipeline looks like:

`source -> tokenize -> parse -> AST transformation passes -> STG -> codegen`

A module's text is tokenized, then parsed into an Abstract Syntax Tree (AST). From there it is
run through a sequence of AST transformation passes. All of the collected module ASTs are combined
and lowered to a Spineless, Tagless G-machine representation (STG), which is then converted to
x86-64 assembly code.

### AST transformation passes (as defined in the module `mirac`):

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

We'll start with the module `name`, which defines the `name` data type used throughout the compiler to specify
variable, function, constructor, and type names. `name`s can be one of:

* `Unqualified`: a name that has not yet been qualified with a module
* `Unresolved`: a name that is qualified by a module in the source code, but hasn't been resolved (verified)
* `Qualified`: a resolved name qualified with its associated module
* `Private`: like `Qualified`, but cannot be exported / imported
* `Builtin`: a resolved name for a builtin function or type
* `Internal`: a resolved, non-top-level name which is qualified with a unique integer

The module also defines a number of common operations on `name`s.

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
focused on. The `ast` itself is defined next, along with auxiliary types `caseSel` and `multSel`. Finally, the `anno` type
alias is defined as a tuple of annotation information that is attached to every `defn`:

* location: a tuple of module name, line number, and column number
* recursive: a bool indicating whether the definition is recursive / mutually-recursive or not
* complexity: an int indicating the rough size (number of AST nodes) of the `defn`, used to inform inlining
* use count: an count of the number of distinct uses of this `defn`, used in inlining, unused warning reporting, and dead-code elimination
* free vars: a set of names that are used as variables in the `defn`, but not defined in the `defn`'s scope
* strictness: an int (bit set) with the individual bits specifying whether the corresponding argument index is strict or not

During the initial design, it was thought that the anno info would only need to be attached to a `defn`, but general error
reporting probably needs to be more fine-grained than that. Also, it would be nice to annotate every AST node with its
inferred type after typechecking. So in the future, the annotation field may be expanded and applied to other ast nodes, or
replaced with something else.

After this, `grammar` goes on to define `showS`, a faster way of appending strings by composing functions, then uses it
to define various functions to pretty-print an `ast`. Then come common operations on `expr`s, `pat`s, `texpr`s, and `defn`s,
Then we define two important map data types that are used frequently throughout the compiler:

* `nameMap`: a map from an Unqualified or Unresolved name to a Qualified Name
* `defnMap`: a map from a Qualified name to a `defn`

Finally, we define a fixed precedence and associativity table for common operators. This will likely be replaced with a
user-definable mechanism later on.

## `ast.m`

The `ast` module (which should probably be named "traversal") defines functions for traversing an `ast`, including
accumulations of information and rewrites of the `ast`.

### Accumulation traversals

An `ast` traversal can be used to collect information from an `ast`. Accumulation traversals for bottom-up, top-down, and
top-down with continuation are defined. The top-down with continuation traversal passes the explicit continuation of
traversing the sub-ASTs of an AST node into the astAccum function, to allow it to do things like short-circuit the traversal
or substitute a custom traversal for the sub-ASTs. Also, an accumulation traversal called `astAccumLex` is defined, in which the
accumulator state passed through the traversal is split into a dynamically-scoped component and a lexically-scoped component.
This allows top-down lexical information (such as location) to be attached to the traversal and automatically removed when the
traversal returns.

### Rewriting traversals

An `ast` traversal can also be used to rewrite an `ast`, passing state information along during the traversal. The state is
handled with a `state` monad (defined in `lib/state`). Rewriting traversals for bottom-up, top-down, top-down with continuation
are defined, and, analogous to `astAccumLex`, an `astRewriteLex` implements top-down rewriting with additional lexical scope state
attached.

## `tokenizer.m`

The `tokenizer` module implements functions to convert a character stream (lazy list of characters) to a `tokloc` stream (lazy list
of tokens along with their source location), by removing whitespace and grouping characters to form a token. The main function is
`tokenizePos`, which takes the current line and column numbers and calls specialized tokenizers based upon the next 1 or 2
characters of the input string. Each of the specialized tokenizers is written in a Continuation Passing Style (CPS) to allow a
called tokenizer to "return" multiple values (e.g. updated row, col, and character stream) to a continuation, without having to
use a tuple to structure/destructure the values. This resulted in a measureable performance improvement, because the tokenizer is
called for each character in the input string.

### Qualified Identifiers and Symbols

The `tokenizeQualified` tokenizer  processes identifiers by first checking to see if the next character immediately following
the ident is a `.`. If so, the identifer is treated as the module name of an Unresolved identifier or symbol, which is tokenized
from the characters following the `.`. If not, the identifier is returned as an Unqualified identifier.

### Handling `$`

The tokenizers handle `$` in two different ways: if the `$` is immediately followed (with no whitespace) by a letter or `_`, it
signifies an identifer that is to be used as an infix operator, e.g. `$div` or `$parser.p_satisfy`, and returns that identifier as
a Tsymbol. Otherwise it is treated as a stand-alone symbol (defined in `stdlib` as the function `apply`) or as part of another
symbol.

### Handling a `#` suffix

`tokenizeStr`, `tokenizeLitInt`, and `tokenizeLitChar` all check to see if the next character after tokenization is a `#`. If so,
`tokenizeStr` appends the `#` as part of the name (to be processed as a normal identifier, but with the explicit `#` suffix).
`tokenizeLitInt` and `tokenizeLitChar` use it to return a different token type (`TprimInt` and `TprimChar`, respectively), to be
used later as unboxed primitive values. A `#` character otherwise is parsed as a stand-alone symbol (defined in `stdlib` as a
prefix operator function to return the length of a list), or as part of another symbol.

### Expanding escape characters in strings

Literal strings are tokenized in `tokenizeString` by repeatedly calling `tokenizeChar`, which handles the correct tokenization of
escaped characters like `\n` or `\x32`.

## `parser.m`

Parsing is split into two separate modules `parser` and `mirandaParser`. `parser` implements the basic parsing combinators
for processing a `tokloc` stream, while `mirandaParser` implements parsing combinators specific to the Miranda2 grammar. This
split allows `parser` to be used in the `predef` module as well (to parse builtin type specifications); otherwise a dependency
loop between the `mirandaParser` and `predef` modules would exist.

### Parser state

The parser is based on `lib/maybeState`, which implements a state monad augmented by a maybe monad. This allows a parser to
return a (`Just` value) upon success, or a `Nothing` upon failure, with `p_bind` (and its associated applicative functors)
automatically short-circuiting the return of any failure in a chain of parser combinators.

The state passed through the parsers, `psSt`, is a tuple of
* the module name
* the deepest error encountered during parsing, so far
* a stack of active indent levels to handle offside-rule processing
* the (lazy) list of tokloc values from the tokenizer

Most of the parser combinators are written in a curried form, with an implicit `psSt` operand at the end (rather than
explicitly in the parser's function definition argument list). Instead, state is handled implicitly by lower-level
parsers (e.g. `p_any`), or by the parser `p_get`, which returns the current state. This, combined with the automatic
early-out handling of parsing failures results in high-level parser combinators which are easier to read. For example, the
parser `p_inParens`:

    p_inParens :: parser * -> parser *
    p_inParens p = p_char '(' >> p << p_char ')'

parses using the parser `p` surrounded by parenthesis tokens, without having to show the automatic handling of conditions
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
number, or, if equal, the severity). This is a heuristic to pick which error to report when all parsing alternatives
have failed, the theory being that the best error to report is likely to be the one that proceeded the furthest
before encountering an error.

#### `p_alt`

The alternative parser, `p_alt` tries to run the primary parser, and, if that fails, runs the alternative parser
with the original parser state ("rewinding" the tokloc stream), except it will update the `psErr` part of the
state with that of the first parser.

#### `p_expected` and `p_unexpected`

The parsers `p_expected` and `p_unexpected` are used to report an error as the alternative parser of a `p_alt`.
`p_expected` takes a string to report as the expected result, and is used in various parsers in `mirandaParser`
to give additional information to a parse error. `p_unexpected` is used as a default alternative parser (mainly
used by `p_guard` and `p_satisfy`) to report an error on failure.

#### `p_guard` and `p_satisfy`

The parser `p_guard` takes a primary parser and a predicate parser, and passes the results of running
the primary parser to the predicate parser. If the predicate parser (the "guard" test) fails, then the
`p_unexpected` parser is run, which fails and runs p_error with the message "unexpected" and the current token.
The parser `p_satisfy` takes a parser and a predicate function, and passes the result of the parser to a
`p_guard` parser, which tests the result with the predicate function, and if it is false, fails, invoking the
`p_unexpected` alternate of the `p_guard`.

### `p_any`

The parser `p_any` is the most basic token parser. It is the one that explicitly handles most of the fundamental
parsing errors (end-of-input, error from tokenizer, and the offside-rule error. Most other parser combinators
use it indirectly.

Most other token parsers are written to use a combination of `p_any` and `p_satisfy`, for example `p_token`, a
parser which expects a particular literal token value:

    p_token :: token -> parser token
    p_token t = p_any $p_satisfy (_eq cmptoken t)

## `mirandaParser.m`

The Miranda2 grammar is parsed in the `mirandaParser` module, which groups parser combinators into:
* expression parsing
* pattern and fnform parsing
* type expression parsing
* data constructor parsing
* definition parsing
* library directive parsing
* top-level module declaration parsing

### Parsing expressions

Expression parsers parse the Miranda2 expression grammar, including:
* variables and constructors
* parenthesized expressions, unit (), and tuples
* list expressions
* list comprehensions
* range expressions
* pre- and post-section expressions
* case expressions
* conditional expressions
* infix expressions

#### Parsing list expressions

List expressions are comma-separated lists of expressions that build nested applications of
`builtinCons` and `builtinNil` functions to build a list at runtime, e.g. the list
`[1, 2, 3]` would parse into the expression `Eap builtinCons 1 (Eap builtinCons 2 (Eap builtinCons 3 builtinNil))`

#### Parsing list comprehensions

List comprehensions are a shorthand for performing possibly nested map and filter operations on lists, e.g.

    [(a, b, c) | a, b, c <- [1 .. 100]; a * a + b * b == c * c] || list of Pythagorean triples

or an iteration of a recurrence:

    [a | (a, b) <- (1, 1), (b, a + b) ..]       || list of Fibonnaci numbers

These are parsed by the parsers `p_generator`, `p_recurrence`, `p_qualifier`, and `p_comprehension`,
and generate an `ElistComp` `ast` node.

#### Parsing range expressions

Range expressions are a list shorthand for an iteration of integer values, e.g.:
* `[1 .. 10]   `  expands to `stdlib.range 1 10`
* `[10, 8 .. 0]`  expands to `stdlib.rangeBy -2 10 0`
* `[0 ..]      `  expands to `stdlib.rangeFrom 0`

These get turned into Eap nodes, applying the appropriate function from stdlib, by the parsers `p_rangePart`,
`p_range`, `p_rangeBy`, and `p_rangeExpr`.

#### Parsing pre-section and post-section expressions

Pre-sections are partial applications of an infix operator, applying a value to the left-hand side of the
operator, e.g. `(1 +)`, while post-sections apply a value to the right-hand side, e.g. `(: [])`. Pre-sections
expand to a partial application of the operator, while post-sections expand to the partial application of
the function stdlib.converse (flip the two operands of a function) to the expression.

#### Parsing case expressions

Miranda2 adds a limited form of case expression to the Miranda grammar, mainly to support directly coding
the case expressions allowed by the STG machine semantics and for calling builtin functions. An example
of such a case expression is:

    case n of
      I# n# -> case n# +# 1# of
                 n2# -> I# n2#

which is strict evaluation of a boxed integer n to an unboxed word n#, adding an unboxed word 1# to
it with the builtinAdd function, and returning the boxed integer result. The limitation is that the
`CaseAlt`patterns can only be either an unboxed word literal, or a constructor with only variable or
wildcard pattern arguments. Parsing of these are handled with the `p_casePat`, `p_caseAlt`, and
`p_caseExpr` parsers, with `p_casePat` checking the pattern of a `CaseAlt` for legality.

#### Parsing conditional expressions

Conditional expressions are expressions on the right-hand side of an equality definition that can be used to
qualify the definition, e.g.:

    r = "zero",    if x == 0
      = "less",    if x < 0
      = "greater", otherwise

These are handled by the parsers `p_cond` and `p_conds`, which use the `p_indent` and `p_outdent` parsers from
the `parser` module to control the layout formatting.

#### Parsing infix expressions

General infix expressions are handled by passing prefix operator, infix operator, and term parsers
to the parameterized parser `p_expExpr` for it to use when parsing those components of a complex
infix expression. This allows `p_expExpr` to be used for parsing both expressions and pattern expressions
by varying the parameterized parsers.

`p_expExpr`, `p_expOp`, `p_expAp`, `p_expReduce`, and `p_expFinal` form a mutually-recursive group of parsers
that operate on a stack of pending expressions and a stack of pending infix operators, using the pre-defined
operator precedence/associativity table from the `grammar` module.

##### `p_expExpr`

This parser parses either a prefix operator, pushing it onto the operator stack and recursively continuing,
or parsing a term, pushing it onto the expression stack and continuing with `p_expOp`.

##### `p_expOp`

This parser parses either an infix operator, pushing it onto the operator stack and continuing with `p_expReduce`,
or trying to parse an application by calling `p_expAp`.

##### `p_expAp`

This parser tries to parse a term, pushing it onto the expression stack and continuing with `p_expOp`, or calls
`p_expFinal`, to perform a final reduction on the pending expression and operator stacks.

##### `p_expReduce`

This parser performs reduction on the expression and operator stacks, based upon the relative precedence and
associativity of the operators on the operator stack. It first tries to reduce prefix operators, including
conversion of a prefix `neg` operator and a literal int to a literal negative int. Binary operators are
reduced similarly, applying the binary operator to the top two expressions on the expression stack. Special
handling is performed on operators with "AssocCompare" associativity: these allow operator "chaining",
converting expressions like: `0 <= x < maxX` into `0 <= x & x < maxX`, automatically inserting calls to stdlib.(&).

##### `p_expFinal`

This parser performs the final reduction steps remaining on the expression and operator stacks, and returns the final
parsed expression.

### Parsing patterns and fnforms

Patterns are used on the left-hand side of an equality definition or a `caseAlt` to destructure an expression value.
`fnform`s are the left-hand side of function definitions: the function name and its argument patterns. Both are
parsed with `p_patOrFnForm`, which uses `p_expExpr`, limiting prefix parsing to `neg` (for integer literals) and
term parsing to `p_formal`: vars, constructors, literals, and paren expressions and list expressions of those.

After successful parsing of a pattern with `p_expExpr`, the parsed expression is walked through `mkPatFn`, which 
ensures all of the arguments in the expr are valid patterns, and then classifies it as a pattern or `fnform`
based upon the name of the first `Evar` and the number of arguments.

### Parsing type expressions

Type expression parsers parse the Miranda2 type expression grammar, including:
* type names and type variables
* parenthesized type expressions, type Unit (), and type tuples
* type list expression (a single type within brackets)

The individual parsers are structured similarly to their value expression counterparts.

### Parsing data constructors

Data constructors are the right-hand side of a Data definition; they are parsed by the `p_construct` and
`p_constructs` parsers. `p_construct` handles parsing individual constructors with arguments (optionally
strict). It also parses infix constructors. `p_constructs` parses a list of these, separated by `p_vBar` (|).

### Parsing Definitions

A Miranda2 definition can be:
* a function definition, e.g. `inc n = n + 1`
* a pattern definition, e.g. `(a, _, b) = x`
* a type definition, e.g. `nameMap == m_map name name`
* a type specification, e.g. `inc :: int -> int`

These are handled by the parsers `p_def`, `p_tdef`, and `p_spec`. In addition to returning the definition upon a
successful parse, these parsers also return a `modInserter`; a function, which when called with a `module`
argument, inserts the definition into the approprate data structure in the module. This allows definitions at
the top-level of the module to be added to the module easily, after being collected into a list of `modInserter`s.

### Parsing Library directives

Miranda2 library directives begin with a `%`:

    %import <fileSpec> {qualified} {as <identifier>} ['-' <identifier> | <identifier> '/' <identifier>]*
    %export ['+' | <fileSpec> | '-' <identifier> | <identifier>]*

They are parsed with the parsers `p_fileSpec`, `p_libPart`, `p_libQual`, `p_libAs`, `p_alias`, `p_env`, and
`p_libDir`. There are also some (deprecated) parsers for handling `%free` and the `%free` directive's `lib_binding`s.

### Parsing top-level `module` declarations

Declarations at the top-level of a `module` are parsed with `p_decl`, and `p_decls` parsers, which try parsing
definitions in the order `p_tdef`, `p_def`, `p_spec`, and `p_libDir`. Type definitions (`p_tdef`) are tried first,
because there is an ambiguity in attempting to parse type definitions and data definitions with the addition of
user-defined infix operators in Miranda2:

    foo == bar  || type alias, but could also incorrectly be parsed, along with
    baz = 42    || this following definition as: (foo == bar) baz = 42, e.g. an inline function `==`.

`p_decls` returns a list of `modInserter`s to be inserted into the `module`.

The two functions exported from the `mirandaParser` module are `parse`, and `parseExpr`. `parse` takes a module
and an input string (lazy list of characters), and calls `p_decls` with an initialized `psSt`. If the parse
is successful; `parse` calls `makeModule` with the `module` and the list of `modInserter`s to insert all of the
top-level declarations. Otherwise, it creates an error with the error info from `psSt` and returns that.

`parse_expr` is exported to allow future parsing of program fragments for, e.g. interactive REPLs.

## `exception.m`

The `exception` module unifies the handling of errors and warnings used throughout the rest of the compiler.
It defines an `exceptionType` to classify exceptions into `Note`s, `Warn`s, or `Error`s, and an
`exceptionInfo` data type to further classify the exception.

### `locInfo`

`locInfo` is a type which pairs a possible `location` (as defined in the `grammar` module) with a hierarchical
"path" of definitions that specify the exact definition which contains the location, used when reporting an
exception to provide additional location information.  Additional functions to create and manipulate `locInfo`
values are defined to create and manipulate `locInfo`s.

### `exception` and `excpt` types

An `exception` is defined as a tuple of `exceptionType`, `exceptionInfo`, and `locInfo`, and an `excpt` is then
defined as either a list of `Error`s (for reporting an error), or a result, along with a list of possible
`Warn`s and `Note`s, used for returning a result along with additional information.

### monad interface for `except`

`excpt`s have a monad interface (and associated functor and applicative interfaces) to allow them to be chained
together with a monadic bind, to automatically handle short-circuiting computation on an `Error` and collecting
and merging `Note` and `Warn` exception information for the chain of `excpt`s.  The monadic bind operation
(`ex_bind`) is written as:

    ex_bind :: excpt * -> (* -> excpt **) -> excpt **
    ex_bind (Left errs)        f = Left errs
    ex_bind (Right (x, warns)) f
        = r, if isLeft r
        = Right (x', warns ++ warns'), otherwise
          where
            r                  = f x
            Right (x', warns') = r

and the monadic pure operation creates an `excpt` with a value and no `Error`s or `Warn`s.

    ex_pure :: * -> excpt *
    ex_pure x = Right (x, [])

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
