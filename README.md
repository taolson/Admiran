# Miranda2 Compiler
{high-level description of Miranda2}

## Language Description


### Miranda features removed from Miranda2
 * ---

### Miranda2 new features not in Miranda

### Differences from Haskell

## Bootstrapping Miranda2
{why it is required, steps (need to edit compiler/config.m!)}

## Files
There is a number of subdirectories:
* `Tools/` a few useful tools for compressions etc.
* `bin/` executables are put here
* `generated/` this contains the (machine generated) combinator file for the compiler.
* `lib/` this contains the `Prelude` and other base library file.
* `src/MicroHs` the compiler source
* `src/runtime` the runtime source
* `tests/` some tests

## Library (move this to a new README.md in lib)?
## Runtime
The runtime system is written in C and is in `src/runtime/eval.c`.
It uses combinators for handling variables, and has primitive operations
for built in types and for executing IO operations.
There is a also a simple mark-scan garbage collector.
The runtime system is written in a reasonably portable C code.

## Compiler (move this to a new README.md in compiler directory)?
The compiler is written in Micro Haskell.
It takes a name of a module and compiles to a target (see below).
This module should contain the function `main` of type `IO ()` and
it will be the entry point to the program.

### Compiler modules

* `Compile`, top level compiler.  Maintains a cache of already compiled modules.
* `Desugar`, desugar full expressions to simple expressions.
* `Exp`, simple expression type, combinator abstraction and optimization.
* `Expr`, parsed expression type.
* `Graph`, strongly connected component algorithm.
* `Ident`, identifiers and related types.
* `IdentMap`, map from identifiers to something.
* `Interactive`, top level for the interactive REPL.
* `Lex`, lexical analysis and indentation processing.
* `Main`, the main module.  Decodes flags, compiles, and writes result.
* `MakeCArray`, generate a C version of the combinator file.
* `Parse`, parse and build and abstract syntax tree.
* `StateIO`, state + IO monad.
* `TCMonad`, type checking monad.
* `Translate`, convert an expression tree to its value.
* `TypeCheck`, type checker.
