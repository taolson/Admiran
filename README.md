# Admiran, a pure, lazy, functional language and compiler

Admiran is a pure, lazy, functional language and compiler, based upon the
original Miranda language designed by David Turner, with additional features
from Haskell and other functional languages.

## System Requirements

Admiran currently only runs on x86-64 based MacOS or Linux systems.  The only external dependency
is a C compiler for assembling the generated asm files and linking them with the C runtime library.
(this is automatically done when compiling an Admiran source file).

## Features

* Compiles to x86-64 assembly language
* Runs under MacOS or Linux
* Whole program compilation with inter-module inlining and optimizations
* Compiler can compile itself (self hosting)
* Hindley-Milner type inference and checking
* Library of useful functional data structures, including
  - map, set, and bag, based upon AVL balanced-binary trees
  - mutable and immutable vectors
  - functor / applicative / monad implementations for maybe, either, state, and io
  - lens for accessing nested structures
  - parser combinators
* small C runtime (linked in with executable) that implements a 2-stage compacting garbage collector
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
* Wildcards in pattern matching
* Typed "holes" to have the type checker report the type of a specified hole in a type spec
or expression
* Case expressions
* Names can be qualified with their module name
* Module imports can be qualified only, or renamed
* Unboxed ints, chars, and strings
* Underscores allowed in integer literals
* Automatically-derived instances of ord (comparison) and show instances for user-defined
  data types and type aliases

### Differences from Haskell

Haskell's design was strongly influenced by Miranda, so they have a lot of similarities.
The main differences are:
* No typeclasses (so no generic Show, Ord, Functor or Monad). Instead, instances
  of a "typeclass" dictionary are passed explicitly to functions that require them
* Admiran typenames are lower-case, and type variables are *, **, etc. instead of lower-case variables
* Data and type definitions use a different syntax (::= and ==, respectively)
* Admiran allows only restricted simple patterns for case alternatives
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

## Examples

Here's a small example of an Admiran program, to generate and print a list of the first 100 primes:

    || primes.am -- generate primes the lazy recursive way
    || From David Turner's original "sieve" example
    
    %import <io>
    
    primes :: [int]
    primes = sieve [2 ..]
             where
               sieve (p : xs) = p : sieve [x | x <- xs; x $mod p ~= 0]
    
    main :: io ()
    main = primes |> take 100 |> showlist showint |> putStrLn

Some small example programs are in the examples directory.  They can be built with the Makefile in that directory, or individually
by typing `amc` *module name* e.g. `amc fib`

Note that amc is a whole-program compiler, so you only need to specify the top-level module that contains the "main" function;
all other required modules will be built as required.

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
