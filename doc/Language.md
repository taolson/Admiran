# Admiran Language Reference

## Introduction

Admiran is primarily based upon the Miranda language written by David Turner. Much of the existing documentation
on writing programs in Miranda is generally applicable to writing programs in Admiran, but there are a number of
key differences:

* `num` type (combined floating-point or arbitrary-precision integers) replaced with 64-bit int
* polymorphic show and comparison operators that work on any type replaced with manual show and
  ord "typeclass" instances and distinct infix operators for comparing ints, chars, and strings
* patterns with repeated variables and "n + k" arithmetic patterns are no longer allowed
* `div` and `mod` functions are now standard functions instead of implicitly infix and must be used as
  `$div` and `$mod` for infix operation
* no `%free` directive for parameterized modules

An overview of Miranda can be found here: [An Overview of Miranda](https://www.cs.kent.ac.uk/people/staff/dat/miranda/Overview.html)

The following reference is based upon the structure and information of the Haskell 2010 Language Reference.

### Program Structure

A Admiran program consists of a collection of *modules*. Modules provide a way to control namespaces and re-use
software in large programs. The top level of a module consists of a collection of *declarations*. Declarations
define functions, values, and types used in the module as well as module imports and exports. At the next lower
level are *expressions*. An expression denotes a *value* and has a static *type*. At the bottom level is Admiran's
*lexical structure*, which defines the individual tokens that make up a Admiran program.

Declarations in a module are unordered: there is no requirement that a declaration used by others must occur before
its uses.

### Values and Types

An expression evaluates to a *value*, and has a static *type*. For example, the declarations

    x :: int
    x = 42

declare a type specification for the definition `x`, which is type `int`, and a value of `x`, which is `42`.

The Hindley-Milner type system allows user-defined type synonyms and data types that can use parametric polymorphism.
For example a user-defined tree data type which can be built from values of any type can be written as:

    tree * ::= Leaf | Node * (tree *) (tree *)

with the `*` being a parametric type variable which can match any type.

### Namespaces

There are 5 different kinds of names in Admiran, which are grouped into 4 namespaces:

* Module names
* Variable names and Type names
* Constructor names
* Type Variable names

#### Module Names

Module names are simple strings which directly map to the *basename* of the module's file path. They are used
in `%import` and `%export` directives, and can be used to qualify variable, constructor, and type names. Module
names can be aliased in an `%import` directive to provide a shorthand for qualifiers.

#### Variable Names and Type Names

Variable names and type names share the same namespace in Admiran. They are either

* Alphanumeric strings which begin with a lower-case letter, `'`, or `_`, e.g.
  * `foo` `alphaBeta5'` `_eq`
* Symbolic character strings which specify infix operator names, e.g.
  * `+` `>>=` `!!`

Alphanumeric variable and type names can be used in an infix manner when preceded directly by a `$`,
e.g. `x $mod 7`. Symbolic variable and type names can be used in a non-infix manner when surrounded by parenthesis,
e.g. `foldl (+) 0 xs`.

Variable and type names can be *qualified* with a module name to disambiguate them in the case of a potential
name clash, or to provide more documentation on the origin of the name, e.g. `stdlib.map foo xs` or
`a vector.+ b`.

#### Constructor names

Constructor names are either

* Alphanumeric strings which being with an upper-case letter, e.g.
  * `Nothing` `Lit` `V2`
* Symbolic character strings beginning with `:` which specify infix constructors, e.g.
  * `:>` `:+:`

Like variable and type names, alphanumeric constructors can be used in an infix manner when preceded directly
by a `$`, and symbolic constructors can be used in a non-infix manner when surrounded by parenthesis.

#### Type Variable Names

A type variable is used to specify a polymorphic type parameter in a type definition or type specification.
They are written as a string of `*` characters, disambiguated by the length of the string. For example,
`either * ** ::= Left * | Right **` defines a data type `either` which has two type parameters `*` and `**`.

## Lexical Structure

The lexical structure of a Admiran program consists of a sequence of *tokens*, separated by whitespace.
Whitespace (spaces, tabs, newlines, and comments) is ignored, except for contributing to the layout rules.

### Comments

Comments in Admiran begin with a token starting with two consecutive vertical bars (i.e. `||`) and
extend to the end of the line. Note that `|||` parses as as a comment and not a symbolic operator, so comments
effectively prevent defining symbolic operators that begin with `||`.

### Identifiers and Operators

Identifiers and operators are variable, type, or constructor names, as specified in the Namespaces section.

### Integer Literals

Integers in Admiran are 64-bit signed machine integers. Integer literals can be specified in

* decimal: `42`, `-1`
* hexadecimal: `0xffff`
* octal:   `077`
* binary:  `0b100100`

In addition, integer literals can use embedded underscores (`_`) as digit separators (with no effect on
the numeric value) to help reading large numbers, e.g. `1_234_567` or `0x1234_5678`

### Character and String Literals

Character literals are written between apostrophes, as in `'a'`, and strings between double quotes, as in
`"Hello"`. Both character and string literals can use escape codes, formed from a backslash (`\`) followed
by a character escape for specifying standard control characters `\a \b \f \n \r \t \v` or to quote
a character itself, e.g. `\\` for a single backslash, or `\"` for a double quote character. Numeric codes
can also be used with a decimal or hexadecimal value to specify an escape character, e.g. `\10` or `\x7f`.

### Unboxed `Word#` Literals

Admiran also provides access to raw, unboxed, 64-bit `word#` values, for use in interfacing to
low-level built-in functions, or to write performance-tuned functions without boxing / unboxing overhead.

Integer `word#` values are written as an integer literal immediately followed by a '#', e.g. `42#`.
Unboxed characters can also be written with character literals, e.g. `'a'#`.

### Layout

Admiran programs are *layout sensitive*, meaning that the correct parsing of a program depends upon how lines
are indented with respect to each other. After a definition symbol (a `=`, `==`, `::=` `::`) in a definition,
or a `%import` or `%export` directive, or the `of` in a `case .. of` expression, the beginning column number
of the following token is captured, and used to disambiguate where the expression ends. If a subsequent line
starts at or beyond the current layout column, it is considered to be a continuation of the current construct.
Otherwise, it signals the end of the current construct and starts a new one. For example:

    x = (3 + y)         || the next token after the "=" sets the
        * 17            || indentation for the rest of the definition
    y = 52              || a new definition, because it starts to the left

    case b of           || the next token after the "of" sets the
      False -> "F"      || indentation for the rest of the case expression
      True  -> "T"
    where
      b = x < y

A semicolon (`;`) can be used to specify the end of a construct instead of indentation:

    x = (3 + y) * 17; y = 52
    case b of False -> "F"; True -> "T"; where b = x < y

## Expressions

A Admiran expression is a sequence of term expressions and function applications
interleaved with infix operators. Term expressions can be:

* a variable, constructor, or literal
* a parenthesized expression
* a presection or postsection
* a list expression
* a tuple expression
* a range expression
* a list comprehension
* a case expression

which are detailed in later sections.

### Operator Precedence and Associativity

Expressions involving infix operators are disambiguated by the precedence and associativity assigned to the operator.
This currently is a fixed table defined in the compiler's `grammar` module, which closely follows Haskell's fixity
definitions for the corresponding operators, and is replicated here:

| op   | prec | assoc   | operation meaning
| -----|------|---------|-----------------------------------------|
| $    |  0   | Right   | function application                    |
| $!   |  0   | Right   | strict function application             |
| \|>  |  1   | Left    | reverse function application / chaining |
| >>=  |  1   | Left    | generic monad bind                      |
| >=>  |  1   | Right   | generic monad Kleisli composition arrow |
| >>   |  1   | Left    | generic monad right                     |
| <<   |  1   | Left    | generic monad left                      |
| :    |  1   | Right   | list constructor                        |
| ++   |  1   | Right   | list append                             |
| --   |  1   | Right   | list difference                         |
| \/   |  2   | Right   | boolean OR                              |
| &    |  3   | Right   | boolean AND                             |
| ~    |  4   | Prefix  | boolean NOT                             |
| <$>  |  4   | Left    | generic functor fmap                    |
| <*>  |  4   | Left    | generic applicative apply               |
| <*   |  4   | Left    | generic applicative left                |
| *>   |  4   | Left    | generic applicative right               |
|      |      |         |                                         |
| >    |  5   | Compare | comparisons for int type                |
| >=   |  5   | Compare |                                         |
| ==   |  5   | Compare |                                         |
| ~=   |  5   | Compare |                                         |
| <=   |  5   | Compare |                                         |
| <    |  5   | Compare |                                         |
|      |      |         |                                         |
| >.   |  5   | Compare | comparisons for char type               |
| >=.  |  5   | Compare |                                         |
| ==.  |  5   | Compare |                                         |
| ~=.  |  5   | Compare |                                         |
| <=.  |  5   | Compare |                                         |
| <.   |  5   | Compare |                                         |
|      |      |         |                                         |
| >$   |  5   | Compare | comparisons for string type             |
| >=$  |  5   | Compare |                                         |
| ==$  |  5   | Compare |                                         |
| ~=$  |  5   | Compare |                                         |
| <=$  |  5   | Compare |                                         |
| <$   |  5   | Compare |                                         |
|      |      |         |                                         |
| .&.  |  5   | Left    | bitwise boolean AND                     |
| .\|. |  5   | Left    | bitwise boolean OR                      |
| .^.  |  5   | Left    | bitwise boolean XOR                     |
| .<<. |  6   | Left    | bit shift left                          |
| .>>. |  6   | Left    | arithmetic bit shift right              |
|      |      |         |                                         |
| +    |  6   | Left    | arithmetic on int type                  |
| -    |  6   | Left    |                                         |
| neg  |  7   | Prefix  |                                         |
| *    |  8   | Left    |                                         |
| div  |  8   | Left    |                                         |
| mod  |  8   | Left    |                                         |
| /    |  8   | Left    |                                         |
| ^    |  9   | Right   |                                         |
|      |      |         |                                         |
| .    | 10   | Right   | function composition                    |
| .>   | 10   | Left    | flipped function composition            |
| #    | 11   | Prefix  | list length                             |
| !    | 12   | Left    | list indexing                           |
| !!   | 12   | Left    | vector indexing                         |

Comparison operators (shown with `Compare` associativity) allow chaining:
`0 <= n < 10` is equivalent to `0 <= n & n < 10`

### Function Application and Curried Applications

*Function application* is written as the juxtoposition of the function name and its arguments,
e.g. `fact 5` or `max (a + 7) b`. Function application has the highest precedence, and is left
associative.

Functions in Admiran are *curried*, which means that functions of multiple arguments can be
thought of as functions of a single argument returning another function. So

    max (a + 7) b

can be written as

    (max (a + 7)) b

where `max` takes a single int argument, and returns a function which accepts the second argument.
Currying allows a partial application of a function to be passed to another higher-order
function. For example, mapping the `max` of `a` to all the values in a list:

    map (max a) [1 .. 10]

### Sections

Infix operators can also be curried using the special notation of *presections* and *postsections*.
A presection, written as ( *e* *op* ), uses the expression *e* as the left-hand side of the binary
operation *op*, and returns a function which takes an argument for the right-hand side. A
postsection, written as ( *op* *e* ), uses the expression *e* as the right-hand side of *op*, and
returns a function which takes an argument for the left-hand side. For example, to add `1` to
every value in a list:

    map (+ 1) xs

Note that (- 1) is parsed as the prefix negation operator `-` on 1, rather than a postsection
on a binary `-`. To perform the later, the standard library function `subtract` can be used.

### Lists

Lists are a built-in recursive data type in Admiran, with two constructors: `[]` and `:`. `[]`
is the empty list, and `:` is an infix constructor appending a value to the head of an existing list.

A List expression is a comma-separated list of expressions within square brackets, e.g.

    [1, 2, 3]                   || equivalent to (1 : (2 : 3 : []))
    ['a', 'b', 'c', 'd']        || equivalent to ('a' : ('b' : ('c' : ('d' : []))))
    [a * 7, b - 3, fst (2, 3)]  || equivalent to (a * 7 : (b - 3 : (fst (2, 3) : [])))

Terms in a list expression must all have the same type; otherwise it is a type error.

String literals in Admiran are represented as a list of `char`: `"Hi!"` is equivalent to
`['H', 'i', '!']`

### Tuples and Unit

Tuples are a built-in data type in Admiran, and can be of any length. Terms in a tuple
expression can be of different types. Tuples are written as comma-separated list of expressions
within parentheses, e.g.

    ('a', 5)
    (x * 3, "Hi " ++ "There", z)
    ([1, 2, 3], "test")

A tuple of zero expressions, `()` is also known as a `Unit` expression. It is a single value `()`
of type `unit` (also represented as `()`). It is commonly used as a return type for I/O or other
state monad operations that only perform side-effects and don't return a value.

### Range Expressions

A *range expression* describes an *int* list with a starting value, an optional increment value
(determined by a second value), and an optional final value, written in the form:
`[` *exp1* [ `,` *exp2* ] `..` [ *exp3* ] `]`

Finite range expressions have an inclusive final value:

    [0 .. 4]    || equivalent to [0, 1, 2, 3, 4] or stdlib.range 0 4
    [9, 7 .. 1] || equivalent to [9, 7, 5, 3, 1] or stdlib.rangeBy (-2) 9 1

Infinite range expressions don't have a final value:

    [0 ..]        || equivalent to stdlib.rangeFrom 0
    [x, x + d ..] || equivalent to stdlib.rangeByFrom d x

### List Comprehensions

A *list comprehension* has the form `[` *exp* `|` *qual1* { `;` *qualN* }* `]`

which collects a list of all *exp* such that *qualifiers* hold. If there are two or more qualifiers,
they are separated by semicolons. Each qualifier is either a *generator*, of which the two forms are:
*pat-list* `<-` *exp*  (first form) or *pat* `<-` *exp1* `,` *exp2* `..` (second form, a recurrence)
or else a *filter*, which is a boolean expression restricting the range of the variables introduced
by preceding generators. The variables introduced on the left of each `<-` are local to the list
comprehension.

#### List comprehension examples

Create an infinite list of square numbers:

    sqs = [n * n | n <- [1 ..]]

List all the factors of a number:

    factors n = [r | r <- [1 .. n $div 2]; n $mod r == 0]

Find all the moves a knight can make from coordinates i j:

    knightsMoves i j = [(i + a, j + b) | a, b <- [-2 .. 2]; a * a + b * b == 5]

Note that the list of variables on the left-hand side of the `<-` is shorthand for multiple generators,
e.g. `i, j <- exp` expands to `i <- exp; j <- exp`.

The second form of generator allows the construction of lists from arbitrary recurrence relations, so
`[x | x <- a, f x ..]` generates the list `[a, f a, f (f a) ..]`

An example of this is a definition of the Fibonacci sequence:

    fibs = [a | (a, b) <- (1, 1), (b, a + b) ..]

A *pattern* on the left-hand side of `<-` can be *refutable*; if the pattern does not match, then the generated
value is silently discarded, as if it had failed a filter qualifier. So to collect all `x` values from pairs of
`(x, y)` where the y value is 0:

    xs = [x | (x, 0) <- pairlist]

Variables introduced on the left-hand size of `<-` can be used in subsequent qualifiers. For example, to collect
the combinations of all elements in a list, two at a time:

    combs xs = [(x, y) | x : ys <- tails xs; y <- ys]

where `tails` returns the successive tails of a list.

### Case Expressions

Case expressions in Admiran are the mechanism for interfacing with built-in functions that operate on unboxed
words, and are evaluated strictly. They can also be used to evaluate an expression strictly and perform a
conditional switch on the resulting constructor. A case expression has the general form:

    case expS of
        pat1 -> exp1
        pat2 -> exp2
        .
        .

where expS (the *scrutinee* expression) is evaluated strictly, and must evaluate to either
* an unboxed `word#` value (for results of built-in functions)
* a saturated constructor value

and the patterns on the left-hand side of the case alternatives are either
* a variable to be bound to the evaluated result
* a literal unboxed `word#` to match with the result to select between alternatives
* a wildcard pattern (`_`), which ignores the corresponding value
* a saturated constructor pattern with only variable or wildcard parameters

A variable pattern can be used to perform low-level built-in operations, e.g.

    case a# +# b# of r# -> I# r#        || built-in addition on unboxed words

A literal unboxed `word#` pattern is typically used when performing a switch on a built-in
cmp# function:

    case a# cmp# 0# of
      0# -> EQ  || cmp# returns 0# if arguments are equal
      1# -> LT  || cmp# returns 1# if arg1 < arg2
      2# -> GT  || cmp# returns 2# if arg1 > arg2

A wildcard pattern can be used to specify a default "otherwise" case alternative which will
always match if all of the previous case alternative patterns fail:

    case cmpint x 0 of
      EQ -> "equal"
      _  -> "not equal"
      
A saturated constructor pattern can be used to evaluate strictly any expression returning
a saturated constructor, e.g.

    case mx of          || switch based upon a maybe int value
      Nothing -> 0
      Just x  -> x + 1

    case tupval of
      (a, _) -> a       || strictly extract the fst component of a tuple

As in definitions and library directives, case expressions are layout sensitive, with the first token
after the `of` setting the indentation for the rest of the case alternatives. Case alternatives
can also be placed on the same line, separated by a semicolon:

    case boolval of False -> "f"; True -> "t"

### Pattern Matching

*Patterns* appear in function definitions, pattern bindings, list comprehensions, and case expressions,
and are used to destructure structured data from lists, tuples, and algebraic data type values.
Patterns look like a saturated constructor application for the particular constructor they are matching
on, or a list expression, with pattern arguments that are either:
* a variable to bind to the corresponding argument's value
* a literal int, char, or string, which must match the corresponding argument's value
* a *wildcard* pattern, which ignores the corresponding argument's value
* another pattern, which is used to further destructure the corresponding argument

Patterns can be *irrefutable* or *refutable*, depending upon their pattern arguments. An
*irrefutable* pattern will always match a value, and consists of a single-constructor data
type with pattern arguments that are one of:
* a variable, e.g. `I# n  || extract the unboxed word# value from an int`
* a wildcard, e.g. `(_, a) || extract snd value of a tuple, ignoring the fst`
* a nested irrefutable pattern, e.g. `(I# n, (_, a))`

Refutable patterns can fail to match, either due to a constructor or list expression not matching,
a literal pattern argument not matching, or a nested refutable pattern argument not matching.
Depending upon the definition or expression in which the pattern is used, a failed match will:
* cause a runtime pattern match error
* proceed to try to match the next pattern
* ignore the value and continue with the next generator value (list comprehension)

#### Pattern matching examples

Define a function "addPair" that takes a tuple argument and adds its components.
A single constructor tuple with all variable patterns is irrefutable:

    addPair (a, b) = a + b

Define a function "map" with two separate pattern matches. The
match on the second value has 2 possible constructors: [] and (:).
If the first match fails, proceed to try the second. Since both constructors
are handled, the overall match is irrefutable.

    map _ []       = []
    map f (x : xs) = f x : map f xs

Destructure a complex nested tuple "p" with a pattern definition.
Binds `a` to the first tuple arg, binds `b` to `snd` part of second tuple arg,
and must match the string `"test"` in the third tuple arg. This pattern
is refutable, due to the pattern match with the literal string `"test"`, and
will cause a runtime error if the pattern fails to match the value in p.

    (a, (_, b), "test") = p

Compare a character with `'a'`, matching on a `GT` / `LT` / `EQ` result.
If the first pattern `GT` fails, the second pattern is the wildcard pattern
`_`, so overall pattern match is irrefutable.

    case cmpchar c 'a' of
        GT -> c - code 'a' + 10
        _  -> c - code '0'

## Function Definitions

A function definition binds a variable to a function value. The general form of a function
definition is *fnvar* *pat0* { *pat1* .. } `=` *rhs*, where *fnvar* is the function variable
name (or a symbolic variable name surrounded by parenthesis) to be bound to the function, *pat0*
 .. *patN* are patterns to be bound to the arguments when the function is applied, and *rhs* is
a *right-hand side* expression which is evaluated in the context of the bound arguments.

Some examples of function definitions:

    square x = x * x

    manhattanDist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

### Function arity and under-applied and over-applied functions

The number of pattern arguments in a function definition define the function's *arity*, which should be
the "natural" number of arguments that the function operates on. A function definition must have at
least one pattern argument (otherwise it is a pattern definition). Functions are curried, and can be
under-applied (calling a function with fewer arguments than its arity dictates) or over-applied (calling
a function with more arguments than its arity dictates). Under-applied functions return a
*partially-applied function*, capturing the supplied arguments and returning a function with an arity
matching the number of missing arguments of the original function. These are useful for passing
partially-applied functions to a higher-order function such as `map`:

    add a b = a + b             || function with "natural" arity 2
    foo = map (add 7) [1 .. 20] || function "add" under-applied to pass to map

A common case for over-applying a function is to define a function with fewer than the required number
of arguments and explicitly return another function:

    subtract b = go where go a = a - b  || function with "natural" arity 1
    x = subtract 7 5                    || subtract over-applied, passing 5 to "go"

This is useful in situations where the function is commonly used as an argument in higher-order functions,
or when a recursive function (which normally cannot be inlined) can be split into a non-recursive part
that captures common arguments and a recursive part, such as the definition of `foldr` in the `stdlib`
module:

    foldr f z
        = go
          where
            go []       = z
            go (x : xs) = f x (go xs)

    sum = foldr (+) 0

The definition of sum calls foldr with its natural arity of 2, allowing sum to inline the foldr definition
and be turned into:

    sum = go
          where
            go [] = 0
            go (x : xs) = x + go xs

`foldr` could also be directly over-applied:

    x = foldr (+) 0 [1 .. 100]

### Functions with multiple definitions

Functions can be defined "piece-wise" using multiple consecutive function definitions with the same
*fnvar* name, and different *pat* pattern arguments. The multiple definitions are conceptually
processed in-order, from top to bottom, until a pattern match for all pattern arguments is found, in
which case the corresponding right-hand side expression is evaluated. If no pattern match is found,
a runtime pattern match error occurs. Each definition in a multiple function definition must have the
same number of pattern arguments, or a compiler Arity error occurs. A common example of a multiple
definition function is matching on the two list constructors`[]` and `:`:

    length []       = 0
    length (_ : xs) = 1 + length xs


## Pattern Definitions

A pattern definition defines one or more pattern variables by destructuring the right-hand side
expression:

    (a, b) = f x        || a and b are bound to the components of the tuple returned from f x
    _ : xs' = xs        || bind xs' to the tail of the list xs (equivalent to xs' = tl xs)

    Efatbar (Pvar n) e1 e2 = expr       || destructure a complex nested expression

If a refutable pattern fails to match, a runtime pattern match error occurs.

## Right-Hand Side Expressions

The simplest form of a right-hand side expression is just an expression, as defined
previously. It is also possible to give several alternative expressions, distinguished by
*guards*, known as a *conditional expression*.

### Conditional Expressions and Guards

A *guard* consists of the keyword `if` followed by a boolean expression. An example
of a right-hand side expression with several alternatives is the gcd function:

    gcd a b = gcd (a - b) b, if a > b
            = gcd a (b - a), if a < b
            = a,             if a == b  || can also be written "otherwise"

Note that the guards are written on the right, following a comma. The layout is significant,
as the offside rule is used to resolve any ambiguities during parsing.

The last guard can be written as `otherwise`, to indicate that this is the case which applies
if all other guards are false. If the last guard is not an `otherwise`, then the entire
expression is refutable, and will fall through to try a following definition (in the case of a 
function with multiple sequential definitions), or cause a runtime error.

A *conditional expression* is of the form `=` *exp* `,` *guard*. Multiple conditional expressions
can be on the right-hand side of a definition, using different guards, and are evaluated from
top to bottom until a matching guard condition is found. Each of the conditional expressions
in a definition must have the same result expression type.

### Nested `where` Definitions

A right-hand side expression is optionally followed by a `where` clause and a set of nested
definitions:

    foo x = p + q, if p < q
          = p - q, otherwise
            where
              p = x * 2 + 1
              q = 3 * x - 5

The nested `where` definitions are local to the enclosing definition. Note that the `where`
definitions themselves, like top-level definitions, may include conditional expressions and
nested `where` definitions.

### Recursive and Mutually-Recursive Definitions

As stated previously, definitions may occur in any order, and may be recursive with themselves,
or mutually-recursive with prior or subsequent definitions that are in-scope. For example:

    even 0 = True
    even x = odd (x - 1)

    odd 0 = False
    odd x = even (x - 1)

Pattern definitions, like function definitions, may also be recursive, as shown in this
example of Admiran's fix-point function:

    fix f = x where x = f x

Here, the value of `x` is defined in terms of calling the function `f` with itself, resulting
in a (lazy) infinite recursive expansion of `f (f (f (f ..)))`

## Type Expressions

Similar to expressions defined previously, Admiran has a similar syntax for *type expressions*.
A type expression is a sequence of type term expressions and type applications interleaved with
infix type constructor operators. Type term expressions can be:

* a type name, e.g. `int`, `char`, `string`
* a type variable, e.g. `*`, `**`
* a fully-saturated type constructor

### Type Constructors and Type Applications

a *type constructor* is a parameterized type name or infix type constructor that allows
polymorphic types to be specified and used. A *type application* is applies a type
constructor to one or more type expressions. Type constructors, unlike data constructors,
must be fully-saturated (cannot be partially-applied). Type applications for list types,
tuple types and function types have special syntax: a list constructor is a type expression
enclosed in square brackets, e.g.:

    [int], [*], [[char]]

A tuple constructor application is a list of comma-separated type expressions enclosed in
parenthesis, e.g.:

    (int, char), ([char], int, (*, int))

An infix function constructor application is a `->` between two type expressions, e.g.

    int -> string               || specifies a function from int to string
    char -> char -> bool        || specifies a function of two chars to a bool

## Module Declarations

Declarations at the top-level of a module consist of
* top-level function and pattern definitions
* type specifications
* type synonym definitions
* Algebraic Data Type definitions
* `%import` and `%export` library directives

### Type Specifications

While type specifications aren't required in Admiran, they can be provided for top-level function and pattern
definitions to help document the definition, as well as to help type inference report more concise errors.

A type specification is of the form *var* `::` *texpr* where *var* is a function
name or variable and *texpr* is a type expression, e.g.:

    (+) :: int -> int -> int

is a type specification for the function `+`, which takes two `int`s and returns an `int`

    map :: (* -> **) -> [*] -> [**]

is a type specification for the function `map`, which is a higher-order function which takes a function
from type `*` to type `**`, and a list of type `*`, and returns a list of type `**`.

    args :: [(int, char)]

is a type specification for the value `args`, which is a list of tuples.

Explicit type specifications can also be used to create stricter types than the most general type that
would be inferred. For example,

    fstInt :: [int] -> maybe int
    fstInt []      = Nothing
    fstInt (x : _) = Just x

would restrict the use of `fstInt` to only operate on lists of ints, while without the explicit
type specification, type inference would infer the most general type as

    fstInt :: [*] -> maybe *

and allow it to be applied to any list.

#### Polymorphic Recursion

There is one situation where explicit type specifications are required: when writing recursive
functions that have *polymorphic recursion*, i.e. where the type parameter can change with each
recursive invocation, instead of being a constant. One example of this is in the library module
`dequeue`, which implements a double-ended queue using *finger trees*:

    dequeue * ::=
        ...
        FTN (dequeue *) (dequeue (dequeue *)) (dequeue *)

The FTN constructor builds a tree with the left and right components being a `dequeue` of the
parametric type variable `*`, but the middle component is a `dequeue` of `dequeue *`. When
writing functions to operate on this data type, e.g.

    dq_size :: dequeue * -> int
    dq_size (FTN l m r) = dq_size l + 3 * dq_size m + dq_size r

the type specification is required, otherwise type checking will report an error that it cannot
unify the type variable `*` with the type `dequeue *`.

### Type Synonyms

A *type synonym* is a way of creating a type name that is an alias for an existing type. It can be used
to shorten more complex types and to better document the intended use of a top-level function or value.
A type synonym is of the form *typeName* { *tvar* } `==` *texpr*, or an infix type synonym of the form
*tvar1* *type_operator* *tvar2* `==` *texpr*:

    string     == [char]                || equates "string" to a list of chars
    errSt      == (string, int, int)    || an error state with an error string and row/col numbers
    m_map * ** == avlTree (*, **)       || polymorphic map type with key and value types
    intMap *   == m_map int *           || a synonym for an m_map with int keys
    * ==> **   == m_map * **            || an infix type synonym for map

Type synonyms are expanded to their equivalent base types during type checking, and type checking errors will
use the equivalent base type when reporting the error.

### Algebraic Data Types

New types can be defined using *algebraic data types*, which introduce a new concrete data type with
specified constructors. Algebraic data types are of the form *var* { *tvar* } `::=` *ctor* { *texpr* } { `|` *ctor { *texpr* } }
where *var* is the data type name, *ctor* is a constructor name, and *texpr* is a type expression. An example
of an algebraic data type is a binary tree of integers:

    intTree ::= Leaf | Node int intTree intTree

This introduces a new data type `tree` with constructors `Leaf` and `Node`. `Leaf` is a constructor with no arguments,
while the `Node` constructor creates a `tree` from an int value and a left and right intTree.

An algebraic data type with a single constructor can be used to define a *record* type, such as:

    person ::= Person string string int || first and last name, age

and can be used to define an *enum* using multiple constructors with no arguments:

    color ::= Red | Orange | Yellow | Green | Blue | Purple

Note that an algebraic data type must have at least one constructor in its definition. To define a new type
with no constructors (used, for example, as a placeholder type for something to be defined in the future),
you can use a type specification with the reserved name `type`, e.g.:

    myType :: type

Then `myType` can be used in subsequent type specifications and type synonyms as if it were a normally-defined
type, until the type is replaced later with an actual concrete type.

#### Polymorphic Data Types

Polymorphic algebraic data types can be defined by introducing type variables on the left and right hand side
of the `::=`. For example, a generalized binary tree would be defined like:

    tree * ::= Leaf | Node * (tree *) (tree *)

Then used polymorphically for different tree types, e.g. `tree char` `tree (int, int)`, etc.

Multiple type variables can be used:

    either * ** ::= Left * | Right **
The constructors defined in an algebraic data type can be used in pattern matching on the type in
both function definitions, pattern bindings, list comprehensions, and case expressions:

    olderThan :: person -> int -> bool
    olderThan (Person _ _ age) limit = age > limit

    stoplightAction :: color -> action
    stoplightAction c = case c of
                          Red    -> Stop
                          Yellow -> Caution
                          Green  -> Go
                          a      -> ReportAction a

#### Strict constructor fields

By default, the fields in a constructor are stored as *thunks*; lazy computations that are
only computed during pattern matching on the field. However, sometimes this can lead to
space leaks, storing long chains of computations waiting to be resolved. To address this,
Admiran allows optional strict fields in data constructors, by appending a `!` to the
corresponding field type in the constructor:

    strictPair ::= SP int! int!

Whenever the constructor SP is used to construct a strict pair, it will first evaluate
any strict arguments, rather than storing them as thunks.

### Automatic Derivation of `ord` and `show`

The Admiran compiler `mirac` will automatically derive functions to compare and show both
type synonyms and algebraic data types, if they aren't explicitly defined. The derived function
names prepend a `cmp` or `show` to the type name, so, for example, defining the data type
`expr` as:

    expr ::= Lit int | Var string | Add expr expr

will also define the functions

    showexpr :: expr -> string
    cmpexpr  :: expr -> expr -> ordering

where `ordering` is a data type defined in the Admiran `stdlib`:

    ordering ::= EQ | LT | GT

`expr`s can then be compared by either using one of the built-in polymorphic comparison functions
`_eq`, `_ne`, `_lt`, `_le`, `_gt`, `_ge`:

    litZero :: expr -> bool
    litZero e = _eq cmpexpr e (Lit 0)

or used in polymorphic functions that perform comparisons for ordering:

    sortedExprs = sortBy cmpexpr exprList

### Import Directives

An *import directive* imports the definitions exported from a module and makes them visible.
It is of the form `%import` *fileSpec* { `qualified` } { `as` *ident* } { *alias* }*
where *fileSpec* is either the name of a module in the standard Admiran library (when enclosed in
angle brackets e.g. `<map>`, or the path to a module relative to the current directory, when
enclosed in double-quotes, e.g. `"parser"`, `"../lib/dequeue"`.

Names are imported *unqualified* by default, meaning that both the qualified name and the
unqualified name can be used. If an import is specified as `qualified`, then the names
are imported as qualified only. Qualified imports allow modules that export the same
name to be imported, because the qualified names don't conflict, even if the unqualified
versions do.

To shorten qualified names, an `as` *ident* can be used; then the alias identifier specified is used instead
of the module name as the qualifier, e.g.

    %import <base> as B

    sortedNames xs = B.sortBy cmpstring xs

instead of

    sortedNames xs = base.sortBy cmpstring xs

Both the `qualified` and `as` options can be used, which means that names must be referred to as the
aliased qualified name.

The import ends with an optional list of space-separated name *aliases*, which allow individual
imported names to either be aliased to a new name, or removed from the import entirely. An alias
is of the form *newName* / *oldName*, where *newName* is the new identifier or operator to be used
in place of the exported name *oldName*. This can be used to modify a name that would otherwise conflict
with one already in scope, or to change a function identifier into an operator, e.g.:

    %import <state> (>>=)/st_bind -st_fmap

would import the `state` module, changing the name of the module's `st_bind` function to the operator
`>>=`, and removing the name `st_fmap` from the import.

An import directive can extend over multiple lines, as long as subsequent lines are indented past the
column where the *fileSpec* begins.

#### `stdlib`

The library module `stdlib` provides a number of standard types, functions, and operators used by most
programs, and used internally in the compiler. It is always implicitly imported in any module, and cannot
be explicitly imported.

### Export Directives

An *export directive* makes top-level definitions of the module available for importing into another module.
It  is of the form `%export` { *libPart* }* where *libPart* is one of

* `+`: export all top-level definitions in this module
* *ident*: add identifier *ident* to the exports
* `-` *ident*: remove identifier from the exports
* *fileSpec*: re-export all exports in module *fileSpec* (which must be imported in this module)

If an export directive is not present, then an implicit `%export +` is assumed.

## Predefined (built-in) Types

The Admiran compiler `mirac` defines a number of built-in base types that cannot be defined by Admiran
type definitions:
* `word#`
* `unit`
* `list` type
* `tuple` types

### `word#`

The type `word#` represents a raw 64-bit machine word, which can represent an unboxed integer or heap
address. Low-level built-in functions operate exclusively on `word#` arguments and return `word#` results.
`word#` values can be used as function or constructor arguments and returned as a value, but cannot be passed
into polymorphic functions, stored in polymorphic data structures, or bound to a variable via a `where`
clause or as part of a more complex expression.

### `unit`

The type `unit` is a type that is inhabited by a single value `Unit`, both represented in code by `()`.
It is the type returned from functions that only perform side-effects and otherwise not return a value,
such as certain IO functions.

### `list` type

The built-in `list` type is a polymorphic type representing a list of a base type. It is equivalent
to the algebraic data type:

    list * ::= Nil | Cons * (list *)

except that it has special syntax for representing and printing:

    [texpr]     || is how the type "list texpr" is specified
    []          || is how "Nil" is specified
    (a : b)     || is how "Cons a b" is specified
    
### `tuple` types

Admiran supports a family of n-ary tuple types which are dynamically created during compilation. An
n-ary tuple type is specified as a comma-separated list of type expressions, enclosed in parenthesis,
and an n-ary tuple value is specified as a comma-separated list of expressions, enclosed in
parenthesis. Since tuple types are built-in and not created via algebraic data type definitions, they
don't have support for the automatic derivation of `ord` and `show` instances. Instead, they must have
manually-written instances. Instances for 2-tuples (pairs) are provided in the `<stdlib>` library,
and instances for n-tuples 3 through 7 are provided in the `<base>` library. The instance
functions are named `showtuple` *n* or `cmptuple` *n*, where *n* is the arity of the tuple, e.g.:

    showtuple3 showint showint showchar (5, 6, 'a')
    cmptuple2 cmpint cmpchar pa pb

If n-ary tuples greater than 7 are used in code, associated `ordI` and `showI` instances must be
provided if the tuple is explicitly compared, or showed.

## Standard Admiran Types

The Admiran standard library `<stdlib>` defines a number of types that are available to use in any module,
and that are used in the Admiran compiler:
* `bool`
* `int`
* `num`
* `char`
* `string`
* `showI *`
* `ordering`
* `ordI *`

### `bool`

`bool` is defined as the algebraic data type `bool ::= False | True`, used in conditional expressions,
comparison operators, boolean *and* `&` and *or* `\/` operators, etc.

### `int` and `num`

`int` is the type which represents a 64-bit integer value, and is defined as the algebraic data type
`int ::= I# word#`, with `word#` being the built-in 64-bit unboxed machine word, and `I#` being
a constructor that boxes a `word#` into an int.

`num` is currently a type synonym for `int`, as Admiran currently does not implement floating-point
values.

### `char`

`char` is the type which represents an ASCII character value, and is defined similarly to `int`:
`char ::= C# word#`, with `C#` being a constructor that boxes a `word#` into a char.

### `string`

`string` is defined as a type synonym `string == [char]`, a synonym for a list of `char` values.

### `showI *`

`showI` is a type synonym for the polymorphic type `* -> string`, representing an "instance" of the
"typeclass" `show`, as described in the section on automatic derivation.

### `ordering` and `ordI *`

`ordering` is defined as the algebraic data type `ordering ::= EQ | LT | GT` used by comparison operators
to report the total ordering of two values of the same type, either equal, less-than, or greater-than.

`ordI` is a type synonym for the polymorphic type `* -> * -> ordering`, which takes two values of the same
type and returns their total ordering, as described in the section on automatic derivation.

## Lazy (Call by Need) and Strict Evaluation

Admiran has Lazy (Call by Need) evaluation by default. Instead of evaluating function arguments before
calling a function, Admiran delays their evaluation by turning them into *thunks*: closures of an
expression to evaluate bundled along with their environment in which the evaluation is to be performed.
Thunks are evaluated on demand, when they are required by the program to determine control flow or as
part of an effect action, such as I/O. When a thunk is evaluated, it is *updated* with its value, which
ensures that a thunk will only be evaluated once, with subsequent evaluations returing the updated value.

Lazy evaluation provides a number of benefits, such as:
* "short-circuiting" control flow operations can be defined as simple functions
* algorithms can sometimes be simplified, or written in a more natural way
* eliminate needless evaluation of a value which isn't required

However, there are some drawbacks, as well. One of the main ones is that thunks take up space in the
heap, and if a lazy computation is continually modified without requiring its evaluation, a chain of
to-be-evaluated thunks can build up, resulting in a space leak. An example of this is the following
function, which attempts to find the length of a list:

    len = go 0
          where
            go n []       = n
            go n (_ : xs) = go (n + 1) xs

In the function `go`, the computation for `n` is lazy: each step of the `go` function creates a new
thunk `(n + 1)` which it passes to the next recursive iteration, resulting in a final value that is
a chain of thunks:

    (((0 + 1) + 1) + 1) ..

To prevent this, `n` should be evaluated strictly. In Admiran, strict evaluation is performed when
* the value is evaluated as part of a conditional expression guard, e.g. `... if n > 0`
* the value is a scrutinee in a case expression
* the value is used in a pattern match that forces its value (i.e. not a simple variable or wildcard)
* the value is the first argument in a `seq` function, which forces evaluation of its first
  argument before returning the second argument as a value.

A rewrite of the problematic function above with strict evaluation of n could be:

    lenStrict
        = go 0
          where
            go n [] = n
            go n (_ : xs) = case n + 1 of n' -> go n' xs

or by using `stdlib.foldl`, which performs strict evaluation of its folded state:

    lenStrict xs = foldl inc 0 xs where inc n _ = n + 1

Many times data structures hold thunks that can result in space leaks. To simplify making them
strict, fields in an algebraic data type can be written with a `!` suffix, which causes the
strict evaluation of that field whenever the constructor is called:

    sizedList * ::= Sized int! [*]

    insert :: * -> sizedList * -> sizedList *
    insert x (Sized n xs) = Sized (n + 1) (x : xs)

Here the computation (n + 1) is strict when the Sized constructor is called.
