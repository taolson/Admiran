# Miranda2 Language Reference

## Introduction

Miranda2 is primarily based upon the Miranda language written by David Turner. Much of the existing documentation
on writing programs in Miranda is generally applicable to writing programs in Miranda2, but there are a number of
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

A Miranda2 program consists of a collection of *modules*. Modules provied a way to control namespaces and re-use
software in large programs. The top level of a module consists of a collection of *declarations*. Declarations
define values and types used in the module as well as module imports and exports. At the next lower level are
*expressions*. An expression denotes a *value* and has a static *type*. At the bottom level is Miranda2's
*lexical structure*, which defines the individual tokens that make up a Miranda2 program.

Declarations in a module are unordered: there is no requirement that a declaration used by others must occur before
its uses.

### Values and Types

An expression evaluates to a *value*, and has a static *type*. For example, the declarations

    x :: int
    x = 42

declare a type specification for the definition `x`, which is type `int`, and a value of `x`, which is `42`.

The Hindley-Milner type system allows user-defined type aliases and data types that can use parametric polymorphism.
For example a user-defined tree data type which can be built from values of any type can be written as:

    tree * ::= Leaf | Node * (tree *) (tree *)

with the `*` being a parametric type variable which can match any type.

### Namespaces

There are 5 different kinds of names in Miranda2, which are grouped into 4 namespaces:

* Module names
* Variable names and Type names
* Constructor names
* Type Variable names

#### Module Names

Module names are simple strings which directly map to the *basename* of the module's file path. They are used
in `%import` and `%export` directives, and can be used to qualify variable, constructor, and type names. Module
names can be aliased in an `%import` directive to provide a shorthand for qualifiers.

#### Variable Names and Type Names

Variable names and type names share the same namespace in Miranda2. They are either

* Alphanumeric strings which begin with a lower-case letter, `'`, or `_` :
  * e.g. `foo` `alphaBeta5'` `_eq`
* Symbolic character strings which specify infix operator names:
  * e.g. `+` `>>=` `!!`

Alphanumeric variable and type names can be used in an infix manner when preceded directly by a `$`,
e.g. `x $mod 7`. Symbolic variable and type names can be used in a non-infix manner when surrounded by parenthesis,
e.g. `foldl (+) 0 xs`.

Variable and type names can be *qualified* with a module name to disambiguate them in the case of a potential
name clash, or to provide more documentation on the origin of the name, e.g. `stdlib.map foo xs` or
`a vector.|+| b`.

#### Constructor names

Constructor names are either

* Alphanumeric strings which being with an upper-case letter
  * e.g. `Nothing` `Lit` `V2`
* Symbolic character strings beginning with `:` which specify infix constructors:
  * e.g. `:>` `:+:`

Like variable and type names, alphanumeric constructors can be used in an infix manner when preceded directly
by a `$`, and symbolic constructors can be used in a non-infix manner when surrounded by parenthesis.

#### Type Variable Names

A type variable is used to specify a polymorphic type parameter in a type definition or type specification.
They are written as a string of `*` characters, disambiguated by the length of the string. For example,
`either * ** ::= Left * | Right **` defines a data type `either` which has two type parameters `*` and `**`.

## Lexical Structure

The lexical structure of a Miranda2 program consists of a sequence of *lexemes*, separated by whitespace.
Whitespace (spaces, tabs, newlines, and comments) are ignored, except for contributing to the layout rules.

### Comments

Comments in Miranda2 begin with a token of two consecutive vertical bars (i.e. `||`) and extend to the
end of the line. Note that `|||` parses as as a comment and not a symbolic operator, so comments
effectively prevent defining symbolic operators that begin with `||`.

### Identifiers and Operators

Identifiers and operators are variable, type, or constructor names, as specified in the Namespaces section.

### Integer Literals

Integers in Miranda2 are 64-bit signed machine integers. Integer literals can be specified in

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
a character itself, e.g. `\\` for a single backslash, or `\"` for a double quote character. Numeric coes can
also be used with a decimal or hexadecimal value to specify an escape character, e.g. `\10` or `\x7f`.

### Unboxed Word# Literals

Miranda2 also provides access to raw, unboxed word# values, for use in interfacing to low-level builtin
functions, or to write performance-tuned functions without boxing / unboxing overhead.

Integer word# values are written as an integer literal immediately followed by a '#', e.g. `42#`.
Unboxed characters can also be written with character literals, e.g. `'a'#`.

### Layout

Miranda2 programs are *layout sensitive*, meaning that the correct parsing of a program depends upon how lines
are indented with respect to each other. After a definition symbol (a `=`, `==`, `::=` `::`) in a definition,
or a `%import` or `%export` directive, or the `of` in a `case .. of` expression, the column number of the
following lexeme is captured, and used to disambiguate where the expression ends. If a subsequent line starts
at or beyond the current layout column, it is considered to be a continuation of the current construct. Otherwise,
it signals the end of the current construct and starts a new one. For example:

    x = (3 + y)         || the next lexeme after the "=" sets the
        * 17            || indentation for the rest of the definition
    y = 52              || a new definition, because it starts to the left

    case b of           || the next lexeme after the "of" sets the
      False -> "F"      || indentation for the rest of the case expression
      True  -> "T"
    where
      b = x < y

A semicolon (`;`) can be used to specify the end of a construct instead of indentation:

    x = (3 + y) * 17; y = 52
    case b of False -> "F"; True -> "T"; where b = x < y

## Expressions

A Miranda2 expression is a sequence of term expressions and function applications
interleaved by infix operators. Term expressions can be:

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

Functions in Miranda2 are *curried*, which means that functions of multiple arguments can be
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

Lists are a builtin recursive data type in Miranda2, with two constructors: `[]` and `:`. `[]`
is the empty list, and `:` is an infix constructor appending a value to the head of an existing list.

A List expression is a comma-separated list of expressions within square brackets, e.g.

    [1, 2, 3]                   || equivalent to (1 : (2 : 3 : []))
    ['a', 'b', 'c', 'd']        || equivalent to ('a' : ('b' : ('c' : ('d' : []))))
    [a * 7, b - 3, fst (2, 3)]  || equivalent to (a * 7 : (b - 3 : (fst (2, 3) : [])))

Terms in a list expression must all have the same type; otherwise it is a type error.

String literals in Miranda are represented as a list of `char`: `"Hi!"` is equivalent to
`['H', 'i', '!']`

### Tuples and Unit

Tuples are a builtin data type in Miranda2, and can be of any length. Terms in a tuple
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
[ *exp1* [, *exp2* ] .. [*exp3* ] ]

Finite range expressions have an inclusive final value:

    [0 .. 4]    || equivalent to [0, 1, 2, 3, 4] or stdlib.range 0 4
    [9, 7 .. 1] || equivalent to [9, 7, 5, 3, 1] or stdlib.rangeBy (-2) 9 1

Infinite range expressions don't have a final value:

    [0 ..]        || equivalent to stdlib.rangeFrom 0
    [x, x + d ..] || equivalent to stdlib.rangeByFrom d x

### List Comprehensions

A *list comprehension* has the form [ *exp* | *qual1* ; ... *qualn* ]

which collects a list of all *exp* such that *qualifiers* hold. If there are two or more qualifiers,
they are separated by semicolons. Each qualifier is either a *generator*, of which the two forms are:
*pat-list* <- *exp*  (first form) or *pat* <- *exp1*, *exp2* .. (second form, a recurrence) or else a
*filter*, which is a boolean expression restricting the range of the variables introduced
by preceding generators. The variables introduced on the left of each `<-` are local to the list
comprehension.

Some examples:

    sqs = [n * n | n <- [1 ..]]                                  || infinite list of square numbers
    factors n = [r | r <- [1 .. n $div 2]; n $mod r == 0]        || list of factors of a number
    knightsMoves i j = [(i + a, j + b) | a, b <- [-2 .. 2]; a * a + b * b == 5]

Note that the list of variables on the left-hand side of the `<-` is shorthand for multiple generators,
e.g. `i, j <- exp` expands to `i <- exp; j <- exp`.

The second form of generator allows the construction of lists from arbitrary recurrence relations, so
`[x | x <- a, f x ..]` generates the list `[x, f x, f (f x) ..]`

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

Case expressions in Miranda2 are the mechanism for interfacing with builtin functions that operate on unboxed
words, and are evaluated strictly. They can also be used to evaluate an expression strictly and perform a
conditional switch on the resulting constructor. A case expression has the general form:

    case expS of
        pat1 -> exp1
        pat2 -> exp2
        .
        .

where expS (the *scrutinee* expression) is evaluated strictly, and must evaluate to either
* an unboxed word# value (for results of builtin functions)
* a saturated constructor value

and the patterns on the left-hand side of the case alternatives are either
* a variable to be bound to the evaluated result
* a literal unboxed word# to match with the result to select between alternatives
* a saturated constructor pattern with only variable or wildcard parameters

The first instance can be used to perform low-level builtin operations, e.g.

    case a# +# b# of r# -> I# r#        || builtin addition on unboxed words

The second instance is typically used when performing a switch on a builtin cmp# function:

    case a# cmp# 0# of
      0# -> EQ  || cmp# returns 0# if arguments are equal
      1# -> LT  || cmp# returns 1# if arg1 < arg2
      2# -> GT  || cmp# returns 2# if arg1 > arg2

The third form can be used to evaluate strictly any expression returning a saturated constructor,
e.g.

    case mx of          || switch based upon a maybe int value
      Nothing -> 0
      Just x  -> x + 1

    case tupval of
      (a, _) -> a       || strictly extract the fst component of a tuple

As in definitions and library directives, case expressions are layout sensitive, with the first lexeme
after the `of` setting the indentation for the rest of the case alternates. Case alternates can also
be placed on the same line, separated by a semicolon:

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

Some examples of patterns:

    || define a function "addPair" that takes a tuple argument and adds its components
    || single constructor tuple with all variable patterns is irrefutable:
    addPair (a, b) = a + b

    || define a function "map" with two separate pattern matches:
    || match on second value has 2 possible constructors: [] and (:),
    || if first match fails, proceed to try the second.  Since both constructors
    || are handled, the overall match is irrefutable.
    map _ []       = []             || ignoring first argument, if second is the empty list, return it
    map f (x : xs) = f x : map f xs || if not, bind the first argument to f, and match a non-empty list pattern

    || destructure a complex nested tuple "p" with a pattern definition
    || binds a to first tuple arg, binds b to snd part of second tuple arg,
    || and must match the string "test" in the third tuple arg. This pattern
    || is refutable, and will cause a runtime error if the pattern fails to
    || match the value in p.
    (a, (_, b), "test") = p

    || compare a character with 'a', matching on GT/LT/EQ indication
    || match on result as 3 possible constructors: GT/LT/EQ.  If the
    || first pattern fails, the second pattern is the wildcard pattern
    || "_", so overall pattern match is irrefutable.
    case cmpchar c 'a' of
        GT -> c - code 'a' + 10
        _  -> c - code '0'

## Definitions and Bindings

### Function and Pattern Bindings

### Nested `where` Definitions

### Conditional Expressions

### Type Specifications

### Algebraic Data Types

### Type Aliases

## Modules

### Module Structure

### Export Directives

### Import Directives

### Separate Compilation

## Predefined Types

### Standard Miranda2 Types

### Strict Evaluation
