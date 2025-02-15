# Miranda2 Language Reference

## Introduction

Miranda2 is primarily based upon the Miranda language written by David Turner. Much of the existing documentation
on writing programs in Miranda is generally applicable to writing programs in Miranda2, but there are a number of
key differences:

* num type (combined floating-point or arbitrary-precision integers) replaced with 64-bit int
* polymorphic show and comparison operators that work on any type replaced with manual show and
  ord "typeclass" instances and distinct infix operators for comparing ints, chars, and strings
* patterns with repeated variables and "n + k" arithmetic patterns are no longer allowed
* `div` and `mod` functions are now standard functions instead of implicitly infix and must be used as
  `$div` and `$mod` for infix operation
* no %free directive for parameterized modules

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

An expression evaluates to a *value*, and has a static *type*. Values and types are not mixed in Miranda2, For example, the declaration

    x :: int
    x = 42

Defines a type specification for the definition `x`, which is `int`, and a value of `x`, which is `42`.

The Hindley-Milner type system allows user-defined type aliases and datatypes that can use parametric polymorphism.
For example a user-defined tree datatype which can be built from values of any type can be written as:

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
in `%import` and `%export` declarations, and can be used to qualify variable, constructor, and type names. Module
names can be aliased in an `%import` declaration to provide a shorthand for qualifiers.

#### Variable Names and Type Names

Variable names and type names share the same namespace in Miranda2. They are either

* Alphanumeric strings which begin with a lower-case letter, `'`, or `_` :
  * `foo` `alphaBeta5'` `_eq`
* Symbolic character strings which specify infix operator names:
  * `+` `>>=` `!!`

Alphanumeric variable and type names can be used in an infix manner when preceded directly by a `$`,
e.g. `x $mod 7`. Symbolic variable and type names can be used in a non-infix manner when surrounded by parenthesis,
e.g. `map (*) xs`.

Variable and type names can be *qualified* with a module name to disambiguate them in the case of a potential
name clash, or to provide more documentation on the origin of the name, e.g. `stdlib.map foo xs` or
`a vector.|+| b`.

#### Constructor names

Constructor names begin with an upper-case letter (or a `:`, in the case of an infix constructor). For example,

    list * ::= Null | * : (list *)

defines a (recursive) data type `list` with two constructors: `Null` and the infix constructor `:`.

#### Type Variable Names

A type variable is used to specify a polymorphic type parameter in a type definition or type specification.
They are written as a string of `*` characters, disambiguated by the length of the string. For example,
`either * ** ::= Left * | Right **` defines a data type `either` which has two type parameters `*` and `**`.

## Lexical Structure

The lexical structure of a Miranda2 program consists of a sequence of *lexemes*, separated by whitespace.
Whitespace (spaces, tabs, newlines, and comments) are ignored, except for contributing to the layout rules.

### Comments

Comments in Miranda2 begin with a token of two consecutive vertical bars (i.e. `||`) and extend to the end of the line.
Note that `|||` parses as an operator, not a comment, because there are more than two vertical bars.

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

### Layout

Miranda2 programs are *layout sensitive*, meaning that the correct parsing of a program depends upon how lines
are indented with respect to each other. After a definition symbol (a `=`, `==`, `::=` `::`) in a definition,
or a `%import` or `%export` declaration, or the `of` in a `case .. of` expression, the column number of the
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
interleaved by infix operators.  Term expressions can be:

* a variable, constructor, or literal
* a parenthesized expression
* a pre-section or post-section
* a list expression
* a tuple expression
* a range expression
* a list comprehension
* a case expression

### Operator Precedence and Associativity

Expressions involving infix operators are disambiguated by the precedence and associativity assigned to the operator.
This currently is a fixed table defined in the compiler's `grammar` module, which closely follows Haskell's fixity
definitions for the corresponding operators, and is replicated here:

| op   | prec | assoc   | operation meaning
| -----|------|---------|-----------------------------------------|
| $    |  0   | Right   | function application                    |
| $!   |  0   | Right   | strict function application             |
| |>   |  1   | Left    | reverse function application / chaining |
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
| .|.  |  5   | Left    | bitwise boolean OR                      |
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
e.g. `fact 5` or `max2 a b`.  Function application has the highest precedence, and is left
associative.

Functions in Miranda2 

### Sections

### Conditionals

### Lists

### Tuples

### Parenthesized Expressions

### Arithmetic Sequences

### List Comprehensions

### Case Expressions

### Datatypes (Types?)

### Pattern Matching

## Declarations and Bindings

### User-Defined Data Types

### Nested `where` Declarations

### Function and Pattern Bindings

## Modules

### Module Structure

### Exports

### Imports

### Separate Compilation

## Predefined Types

### Standard Miranda2 Types

### Strict Evaluation
