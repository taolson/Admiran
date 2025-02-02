# Brief Miranda2 Language Reference

## Miranda

## Literal Constants
Miranda2 has 3 types of literal constants:
  * 64-bit integer, e.g. 1, 0xff, 123_456_789
  * a character, e.g. 'x', '\n', '\012'
  * a string, e.g. "Hello, World!", "This has a \n newline in it."
as well as unboxed versions of those, which map to raw 64-bit words (specified by a '#' suffix):
  * 64-bit word, e.g. 0#, 0xffff#
  * char-valued word, e.g. '\n'# (== 10#)
  * a packed byte array, e.g. "Test"#

## Expressions
An expression is one of
* a simple (atomic) expression
* a function application
* an infix operator expression

### Simple Expressions
A simple expression is one of
* an identifier (starts with a lower-case letter)
* a constructor (starts with an upper-case letter)
* a literal
* a finite list of the same type, e.g. [1, 2, 3], ["this", "is", "a", "list"]
* a tuple of various types, e.g. ('a', 52, 
* a range of integer values (see Ranges)
* a list comprehension

#### Operator Sections
An operator section is a partial-application of an infix operator in parenthesis,
which can be used as an operand to pass to a higher-ordered function, or applied
to another value.  Examples are (7 +), a pre-section of (+) which returns a function
and adds 7 to it, or (: []), a post-section of (:) which takes a value and makes a
singleton list out of it, and (&), which is how to pass the boolean AND infix operator
as a function value.

#### Ranges
integer ranges can be specified as a list, with a starting value, an optional second value
to specify a step size, a "..", and an optional final value:

    [1 .. 10]           || range from 1 to 10, inclusive
    [10, 9 .. 1]        || reverse of above
    [n, n + 5 ..]       || infinite list starting at value of n and increasing by 5 each step

#### List Comprehensions

## Infix Operators

## Identifiers

## Literals

## Layout Rules

## List Comprehensions

## Definitions

## Pattern Matching

## Types

### Type Synonyms

### Algebraic Data Types

## Modules

## State and I/O Monads

