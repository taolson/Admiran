# Admiran, a pure, lazy, functional language and self-hosting compiler

Admiran is a pure, lazy, functional language and self-hosting compiler, based upon the
original Miranda language designed by David Turner, with additional features
from Haskell and other functional languages.

## Examples

Here's a small example of an Admiran program, to generate and print a list of the first 100 primes:

    || primes.am -- generate primes the lazy recursive way
    || From David Turner's original "sieve" example
    
    || io functions which communicate with the outside world (like putStrLn) are defined in the <io> module
    %import <io>
    
    primes :: [int]             || an optional type spec for primes, which is a list of ints
    primes = sieve [2 ..]       || defines primes as the result of calling the sieve function on an
                                || infinite list of sequential integers, starting from 2
             where
               || the sieve function deconstructs the list into its head (p) and tail (xs)
               || and returns p (the next prime) followed by a recursive call to sieve
               || on a new list made from a list comprehension, which filters the remaining list
               || for only the values that aren't divisible by p
               sieve (p : xs) = p : sieve [x | x <- xs; x $mod p ~= 0]
    
    || main takes the first 100 values from the infinite list of primes, converts that list to
    || a string, and prints it, chaining together the functions using the `|>` reverse-apply
    || function defined in the <stdlib> module (implicitly imported).
    main :: io ()
    main = primes |> take 100 |> showlist showint |> putStrLn

Some other small example programs are in the examples directory.  They can be built with the Makefile in that directory, or individually
by typing `amc` *module name* e.g. `amc fib`

Note that amc is a whole-program compiler, so you only need to specify the top-level module that contains the "main" function;
all other required modules will be built as required.

## System Requirements

Admiran currently only runs on x86-64 based MacOS or Linux systems.  The only external dependency
is a C compiler for assembling the generated asm files and linking them with the C runtime library.
(this is automatically done when compiling an Admiran source file).

## Features

* Compiler can compile itself (self hosting)
  - ~6700 SLOC for compiler
  - ~3300 SLOC for library
* Compiles to x86-64 assembly language
* Runs under MacOS or Linux
* Whole program compilation with inter-module inlining and optimizations
* Hindley-Milner type inference and checking
* Library of useful functional polymorphic data structures, including
  - lists and tuples (built-in)
  - map, set, and bag, based upon AVL balanced-binary trees
  - mutable and immutable vectors
  - functor / applicative / monad implementations for maybe, either, state, and io
  - lens for accessing nested structures
  - parser combinators
* Small C runtime (linked in with executable) that implements a 2-stage compacting garbage collector
* 20x to 50x faster than the original Miranda compiler/combinator interpreter

### Miranda language features removed from Admiran

Admiran is an "extended subset" of Miranda, and does not (currently) implement every feature
in the original Miranda language:
* `num` type (combined floating-point or arbitrary-precision integers) replaced with 64-bit `int`
* polymorphic show and comparison operators that work on any type replaced with manual show and
  ord "typeclass" instances and distinct infix operators for comparing ints, chars, and strings
* no `%free` directive for parameterized modules

### Admiran new language features not in Miranda

* Monadic IO scheme instead of sys_message streams
* User-defined infix operators and infix constructors
* Case expressions
* Lambda expressions
* Wildcards in pattern matching
* Generalized partial application of functions using wildcard placeholder variables
* Names can be qualified with their module name
* Module imports can be qualified only, or renamed
* Unboxed ints, chars, and strings
* Underscores allowed in integer literals
* Automatically-derived instances of ord (comparison) and show instances for user-defined
  data types and type aliases
* Type "holes" to have the type checker report the type of a specified hole in a type spec

### Differences from Haskell

Haskell's design was strongly influenced by Miranda, so Admiran has a lot of similarities
with it. The main differences are:
* No typeclasses (so no generic Show, Ord, Functor or Monad). Instead, instances
  of a "typeclass" dictionary are passed explicitly to functions that require them
* Admiran typenames are lower-case, and type variables are *, **, etc. instead of lower-case variables
* Data and type definitions use a different syntax (::= and ==, respectively)
* Admiran allows only restricted simple patterns for case alternatives
* Admiran conditional expressions `= <expr>, if <test>` vs Haskell guarded
  expressions `| <test> = <expr>`
    
* Some layout and offside-rule differences

## Distribution Subdirectories

* `bin/` executables are put here
* `boot/` contains the asm source for the two pre-built bootstrap compilers (one for Linux, one for MacOS)
* `doc/` project documentation (mostly incomplete, in-progress right now)  ToDo list
* `compiler/` contains the Admiran source files for the amc compiler
* `lib/` contains the sources for the various libraries, and the runtime.c file
* `examples/` contains some example programs to show Admiran syntax and to try the compiler out
* `tools/` contains some tools built with Admiran for use on Admiran files

## Configuring and Bootstrapping Admiran

The Admiran compiler (amc) is written in Admiran, and requires bootstrapping from a pre-built
bootstrap compiler.  This is mostly automated in the Makefile, but needs a manual configuration step first:

Edit the config.am file in the compiler directory to modify the values for:

    hostOS                = Linux         || set to Linux or MacOS
    admiranLibPath        = "../lib"      || set to absolute path name for the lib directory, e.g.
                                          || "/home/tim/Programming/Admiran/lib"

then go back to the top-level directory and type "make".

The script will bootstrap the compiler in 4 stages:
1. compile the correct amcBoot asm with the runtime and install in the bin directory (amcBoot)
2. compile the compiler sources and libraries with this bootstrap compiler (amcStage1). This compiler
   is now configured correctly, but was built with the reduced-functionality bootstrap compiler (no
   typecheck or inline passes), so it needs to be rebuilt again with itself to enable those features.
3. re-compile with amcStage1 to create amcStage2.
4. re-compile with amcStage2, to verify that the compiler is stable (produces the same asm file), and install in bin as amc

When complete, it should report
`=== amc compiler built successfully ===`
and install as amc in the supplied bin directory.

It is suggested that you add the bin directory to your PATH variable in your shell, to allow the amc compiler to be run from anywhere.

## .x2 File Extensions

After the amc compiler builds a module (before whole-program merging), it creates a ".x2" file for the module.  This is
a serialized version of the internal optimized Abstract Syntax Tree (AST) of the module, which can be loaded by the compiler
instead of recompiling from source, again.  The compiler checks the corresponding modification times of the .am and .x2 files
to see if the .x2 file is up-to-date, and will re-build from the .am file if it is newer.  The .x2 files can be removed to
force a rebuild from the source file.

The program `tools/dumpX2.am` can be used to pretty-print the contents of .x2 files and show the final result of
the inlined and optimized modules.

## Why did I write this?

To learn more about how functional languages are implemented.  To have a fun project to work on that can provide
a nearly endless list of ToDos (see doc/TODO!).  To have a fun language to write Advent Of Code solutions in.  Maybe
it can be useful for someone else interested in these things.
