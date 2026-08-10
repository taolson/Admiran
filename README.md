# Admiran: a pure, lazy, functional language and self-hosting compiler

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
by typing `amc` *module name* e.g. `amc fib`. Note that amc is a whole-program compiler, so you only need to specify the top-level module
that contains the "main" function; all other required modules will be built as required.

Hundreds of other examples can be found in my [10 years of Advent of Code solutions](https://github.com/taolson/advent-of-code), and the
amc compiler itself is an example of a large, multi-module Admiran program.

## Language Features

Admiran is an "extended subset" of Miranda, an ML-like language,
and has all its basic features:

* Call-By-Value (Lazy) evaluation by default
* Strongly-typed with parametric polymorphism
* Curried, higher-order functions
* Algebraic Data Types
* Abstract types
* Type synonyms
* List comprehensions and infinite lists
* Tuples
* Pattern matching, including nested complex patterns
* Guarded equations
* Layout-sensitive syntax
* Nested function closures
* Modules for controlling name spaces and visibility

In addition, Admiran has features found in other functional languages,
like Haskell and F#:

* Monadic IO scheme instead of sys_message streams
* User-defined infix operators and infix constructors
* Case expressions (strictly-evaluated in Admiran)
* Lambda expressions
* Wildcards in pattern matching
* Names can be qualified with their module name
* Module imports can be qualified only, or renamed
* Unboxed ints, chars, and strings
* Underscores allowed in integer literals
* Automatically-derived instances of ord (comparison) and show functions for user-defined
  data types and type aliases
* Type "holes" to have the type checker report the type of a specified hole in a type spec

## Compiler Features

The Admiran compiler is written in Admiran (self-hosting). The goal is to implement the
entire compiler pipeline, from source tokenization through to low-level asm code generation,
while keeping the compiler small enough (currently ~7000 SLOC in 26 modules) to be easily
understandable by someone studying it.

* Whole-program compilation
* Parser written using parser-combinators
* Desugaring to simplified core AST
* Multiple analysis passes to analyze definitions for free variables, usage, complexity,
  and escape status for let-bound definitions
* Hindley-Milner type inference and checking
* Multi-pass AST optimizer, including:

  - inter-module inlining
  - compile-time evaluation of builtin functions and case selection for known constant operands
  - let floating and case expression floating to expose more optimization opportunites
  - dead-code elimination

* AST serialization / deserialization for modules
* Spineless Tagless G-Machine (STG) IR

  - Implements "Eval/Apply" model
  - Lowers to virtual STG instruction set (register-based)
  - Function call arity analysis and optimization for known functions or closures
  - Tail call optimization
  - Thunk update removal
  - Reachability analysis to only include used definitions (tree-shaking)

* code produced is 20x to 50x the performance of the original Miranda compiler/combinator interpreter

## Library Features

The Admiran library implements many useful functional polymorphic data structures,
useful in both the compiler as well as user programs:

  - Map, Set, and Bag, based upon AVL balanced-binary trees
  - Mutable and immutable vectors
  - Functor / Applicative / Monad implementations for maybe, either, state, and io
  - Lenses for accessing nested structures
  - BitSets for handling sets of small integers
  - Parser combinators
  - Streams (streaming interface that supports stream fusion)
  - Double-ended queue based upon finger trees
  - Heap (priority queue)
  - 2D and 3D vectors with associated math operations, folds, maps, etc.
  - A-star and BFS search algorithms
  - Function memoization
  - Zipper to provide a cursor position within a list
  - Bidirectional "time-traveling" Tardis state monad
  - Small C runtime (linked in with executable) that implements a 2-generation compacting garbage collector

## System Requirements

Admiran currently only runs on x86-64 based MacOS or Linux systems (or under Rosetta 2 on Apple silicon).
The only external dependency is a C compiler for assembling the generated asm files and linking them with
the C runtime library. This is automatically done when compiling an Admiran source file.

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

When complete, it should report `=== amc compiler built successfully ===`
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

## Distribution Subdirectories

* `bin/` executables are put here
* `boot/` contains the asm source for the two pre-built bootstrap compilers (one for Linux, one for MacOS)
* `doc/` project documentation (language documentation complete, others in-progress)
* `compiler/` contains the Admiran source files for the amc compiler
* `lib/` contains the sources for the various libraries, and the runtime.c file
* `examples/` contains some example programs to show Admiran syntax and to try the compiler out
* `tools/` contains some tools built with Admiran for use on Admiran files

## Why did I write this?

To learn more about how functional languages are implemented.  To have a fun project to work on that can provide
a nearly endless list of to-dos. To have a fun language to write Advent Of Code solutions in.  Maybe
it can be useful for someone else interested in these things.

Lovingly hand-crafted with no AI.
