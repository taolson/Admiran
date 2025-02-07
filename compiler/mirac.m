|| mirac -- top level compiler


%export +

%import <either>
%import <io>                    (>>=)/io_bind (<$>)/io_fmap (>>)/io_right
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "analyze"
%import "ast"
%import "codegen"
%import "config"
%import "demand"
%import "dependency"
%import "derive"
%import "desugar"
%import "exception"
%import "grammar"
%import "inline"
%import "mirandaParser"
%import "module"
%import "name"
%import "normalize"
%import "predef"
%import "reify"
%import "rename"
%import "rewrite"
%import "serialize"
%import "stg"
%import "typecheck"


|| turn an excpt * into an io *
io_excpt :: excpt * -> io *
io_excpt (Left errs)        = io_mapM_ (errStrLn . showException) errs  >> exit 1       || print the errors and exit
io_excpt (Right (r, warns)) = io_mapM_ (errStrLn . showException) warns >> io_pure r    || print the warnings and continue with the result


|| serialize a module to an ".x2" file
serialize :: globalEnv -> io globalEnv
serialize genv
    = writeFile fx (serialModule m []) >> io_pure genv'
      where
        m     = view genvMod genv
        fs    = view modFile m
        fx    = withSuffix "x2" (fullFileName fs)
        genv' = over genvMod (set modBuildStatus Serialized) genv

|| either parse the .m file or deserialize the .x2 file of a fileSpec, depending
|| upon their relative modification times
parseOrDeserial :: fileSpec -> io module
parseOrDeserial f
    = mtimeFile fm >>= exists
      where
        fn         = fullFileName f
        fm         = withSuffix "m" fn
        fx         = withSuffix "x2" fn
        initModule = emptyModule f builtinEnv
        doDeserial = io_pure . fst . deserialModule
        doParse    = io_excpt . parse initModule

        exists tsm
            = io_excpt (ex_error err emptyLocInfo), if tsm < 0
            = mtimeFile fx >>= x2exists tsm,        otherwise
              where
                err = Info $ fm ++ " required, but not found"

        x2exists tsm tsx
            = readFile fx >>= doDeserial,                              if tsx >= tsm
            = putStrLn ("parsing  " ++ fn) >> readFile fm >>= doParse, otherwise

parseFiles :: [fileSpec] -> io moduleMap
parseFiles files
    = go m_empty files
      where
        go modules [] = io_pure modules
        go modules (f : fs)
            = go modules fs,                      if m_member cmpstring mn modules    || already parsed
            = parseOrDeserial f >>= addIncs,      otherwise
              where
                mn = fileSpecName f
                addIncs mdl
                    = go modules' (fs ++ includes)
                      where
                        modules'  = m_insert cmpstring mn mdl modules
                        includes  = modImportFiles mdl

|| build a module and insert into the module map, by making a globalEnvironment
|| and passing that through a sequence of compilation passes
buildModule :: moduleMap -> string -> io moduleMap
buildModule mm n
    = updateModMap <$> (io_foldM doPass genv passes >>= serialize)
      where
        Just m        = m_lookup cmpstring n mm
        updateModMap  = view genvModMap . updateEnv . over genvMod (set modBuildStatus Built)
        notData d     = ~isDefData d & ~isConstructor (defnName d)
        doPass genv p = io_excpt (p genv)
        genv          = (m, mm, 20)       || the gen value must be larger than the largest number of free variables in the type signature for any function,
                                          || otherwise the requirement that new free vars cannot conflict with any existing ones during type checking
                                          || can be violated
        passes =
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

|| build the modules in dependency order
build :: [string] -> moduleMap -> io moduleMap
build stdMods mm
    = io_excpt (ex_error (DepCycle (showCycles cyc)) emptyLocInfo), if isLeft morder
    = io_foldM bm mm order',                                        otherwise
      where
        morder      = dependencyOrder cmpstring . makeDependencyGraph cmpstring . m_toList . m_fmap (map fileSpecName . modImportFiles) $ mm
        Left cyc    = morder
        Right order = morder
        order'      = stdMods ++ order       || force stdMods to be built first, as there are implicit dependencies on them

        showCycles cs
            = shows "module includes: " . interShows ", " (map showsCycle cs) $ ""
              where
                showsCycle ns = showsInParens (interShows ", " (map shows ns))

        bm mm mn
            = putStrLn ("building " ++ mn) >> buildModule mm mn, if _eq cmpbuildStatus bs Unbuilt
            = io_pure mm,                                        otherwise
              where
                bs = view modBuildStatus . fromJust . m_lookup cmpstring mn $ mm

compile :: [string] -> io moduleMap
compile files
    = parseFiles (map mkStd stdMods ++ map fileSpecFromString files) >>= build stdMods
      where
        stdMods  = [stdlibModuleName, streamModuleName], if useStreamFusion
                 = [stdlibModuleName],                   otherwise
        mkStd s  = FileSpec stdlibPath s

|| generate an asm file for the module specified by "s" with entry function name "entry"
|| then compile and link it with the runtime code
gen :: string -> string -> io ()
gen s entry
    = compile [fn] >>= genStg >>= reportInsns >>= genCode >> link >> io_pure ()
      where
        fs     = fileSpecFromString s
        fn     = fullFileName fs
        mn     = fileSpecName fs
        qentry = Qualified mn entry
        qstart = Qualified mn "!start"
        asm    = withSuffix "s" fn
        rt     = fullFileName $ FileSpec stdlibPath runtimeName

        || generate the STG code from the module map
        genStg mm
            = putStr "generating STG code " >> io_pure (buildStg qentry genv)
              where
                m      = fromJust $ m_lookup cmpstring mn mm
                genv   = (m, mm, 0)

        || tally total insns generated to force evaluation of build before gen
        reportInsns stg
            = putStrLn (showint n ++ " STG insns") >> io_pure stg
              where
                (_, c, _) = stg
                n         = sum . map (length . snd) . m_elems . fst $ c

        || generate the asm code
        genCode stg
            = putStrLn "generating asm code" >> (writeFile asm . codegen qstart) stg

        || link with the runtime
        link = putStrLn "linking with runtime" >> systemCmd (intercalate " " [cCompilerName, ccOpts, "-o", fn, asm, rt])

main :: io ()
main
    = getArgs >>= go
      where
        go [fn]        = gen fn "main"
        go [fn, start] = gen fn start
        go _           = error "usage: mirac <file name> {entry name}"
