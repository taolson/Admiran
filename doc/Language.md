# Miranda2 Language Reference

## Miranda2 and Miranda

Miranda2 is primarily based upon the Miranda language written by David Turner.  Much of the existing documentation
on writing programs in Miranda is generally applicable to writing programs in Miranda2, but there are a number of
key differences:

* num type (combined floating-point or arbitrary-precision integers) replaced with 64-bit int
* polymorphic show and comparison operators that work on any type replaced with manual show and
  ord "typeclass" instances and distinct infix operators for comparing ints, chars, and strings
* no %free directive for parameterized modules

An overview of Miranda can be found here: [An Overview of Miranda](https://www.cs.kent.ac.uk/people/staff/dat/miranda/Overview.html)

The following reference is based upon the structure and information of the Haskell 2010 Language Reference.

### Program Structure

A Miranda2 program consists of a hierarchical collection of *modules*.  Modules provied a way to control namespaces and re-use
software in oarge programs.  The top-level module consists of a collection of *declarations*.  Declarations define values and
types used in the module, and potentially exported to other modules.

### Values and Types

An expression evaluates to a *value*, and has a static *type*. Values and types are not mixed in Miranda2. The Hindley-Milner
type system allows user-defined type aliases and datatypes that can use parametric polymorphism.

### Namespaces

There are 5 different kinds of names in Miranda2, which are grouped into 4 namespaces:

* Module names
* Variable names and Type names
* Constructor names
* Type Variable names

#### Module Names

Module names are simple strings which directly map to the *basename* of the module's file path.  They are used
in `%import` and `%export` declarations, and can be used to qualify variable, constructor, and type names.  Module
names can be aliased in an `%import` declaration to provide a shorthand for qualifiers.

#### Variable names and Type names

Variable names and type names share the same namespace in Miranda2.  They are either

* Alphanumeric strings which begin with a lower-case letter, `'`, or `_` :
  * `foo` `alpha5'` `_eq`
* Symbolic character strings which specify infix names:
  * `+` `>>=` `!!`

Alphanumeric variable and type names can be used in an infix manner when preceded directly by a `$`,
e.g. `x $mod 7`. Symbolic variable and type names can be used in a non-infix manner when surrounded by parenthesis,
e.g. `map (*) xs`.

Variable and type names can be *qualified* with a module name to disambiguate them in the case of a potential
name clash, or to provide more documentation on the origin of the name, e.g. `stdlib.map foo xs` or
`a vector.|+| b`.

#### Constructor names

Constructor names begin with an upper-case letter (or a `:`, in the case of an infix constructor).  For example,
`list * ::= Null | * : (list *)` defines a data type `list` with two constructors: `Null` and the infix constructor
`:`.

#### Type Variable Names

A type variable is used to specify a polymorphic type argument in a type definition or type specification.
They are written as a string of `*` characters, disambiguated by the length of the string.  For example,
`either * ** ::= Left * | Right **` defines a data type `either` which has two type arguments `*` and `**`.

## Lexical Structure

The lexical structure of a Miranda2 program consists of a sequence of *lexemes*, separated by whitespace.
Whitespace (spaces, tabs, newlines, and comments) are ignored, except for contributing to the layout rules.

### Comments

Comments in Miranda2 begin with two consecutive vertical bars (i.e. `||`) and extend to the end of the line.

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
`"Hello"`.  Both character and string literals can use escape codes, formed from a backslash (`\`) followed
by a character escape for specifying standard control characters `\a \b \f \n \r \t \v` or to quote
a character itself, e.g. `\\` for a single backslash, or `\"` for a double quote character. Numeric coes can
also be used with a decimal or hexadecimal value to specify an escape character, e.g. `\10` or `\x7f`.

### Layout

Miranda2 programs are *layout sensitive*, meaning that the correct parsing of a program depends upon how lines
are indented with respect to each other.  After a definition symbol (a `=`, `==`, `::=` `::`) in a definition,
or a `%import` or `%export` declaration, or the `of` in a `case .. of` expression, then column number of the
following lexeme is captured, and used to disambiguate where the expression ends.  If a subsequent line starts
at or beyond the current layout column, it is considered to be a continuation of the current construct.  Otherwise,
it signals the end of the current construct and starts a new one. For example:

    x = (3 + y)         || the next lexeme after the "=" sets the
        * 17            || indentation for the rest of the definition

    case b of           || the next lexeme after the "of" sets the
      False -> "F"      || indentation for the rest of the case expression
      True  -> "T"
    where
      b = x < y

A semicolon (`;`) can also be used to specify the end of a construct:

    x = (3 + y) * 17; y = 52
    case b of False -> "F"; True -> "T"

## Expressions

### Variables, Constructors, Operators, and Literals

### Curried Applications

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
