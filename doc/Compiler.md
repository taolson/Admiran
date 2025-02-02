# A tour of the mirac compiler internals
The mirac compiler comprises a hierarchy of 26 modules, each performing a specific function in the compiler pass pipeline
from source file to asm binary.  We'll tour these in a bottom-up order, based upon when they are used in the pipeline.

The overall pipeline looks like:

`source > tokenize > parse`: go from source code file of a module to its initial AST representation

`AST pipeline passes (as defined in mirac.m):`

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

`ASTs > stg`: convert all module ASTs to a single Spineless, Tagless G-machine (STG) representation

`stg > codegen`: convert the STG representation to x86-64 assembly language

## grammar.m
We'll start with `grammar.m`, which defines the AST for Miranda2 that will be used and refined in all the subsequent passes.
