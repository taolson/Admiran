|| module.m -- Miranda top-level library modules


%export +

%import <either>
%import <lens>
%import <map>
%import <maybe>                 (>>=)/mb_bind 
%import <mirandaExtensions>
%import <set>
%import "ast"
%import "config"
%import "dependency"
%import "exception"             (>>=?)/ex_bind
%import "grammar"
%import "name"
%import "predef"


|| fileSpec used in imports and exports

fileSpec ::= FileSpec [string] string   || directory path, file name

fileSpecPath, fileSpecName, fullFileName :: fileSpec -> string
fileSpecPath (FileSpec ps n) = intercalate filePathSeparator ps
fileSpecName (FileSpec ps n) = n
fullFileName (FileSpec ps n) = intercalate filePathSeparator $ ps ++ [n]

fileSpecFromString :: string -> fileSpec
fileSpecFromString s
    = FileSpec ps $ withoutSuffix "m" n
      where
        Just (ps, n) = viewR . split (hd filePathSeparator) $ s

|| %export directive
libPart ::=
    LibIdent   name     location |      || export this identifier
    LibRemove  name     location |      || remove this identifier from the export
    LibFile    fileSpec location |      || export all identifers exported from this module
    LibAll              location        || export all identifiers in this module (default, if no export directive)

isLibIdent :: libPart -> bool
isLibIdent (LibIdent n loc) = True
isLibIdent lp               = False

isLibRemove :: libPart -> bool
isLibRemove (LibRemove n loc) = True
isLibRemove lp                = False

|| qualification spec for imported identifiers
libQual ::= LibUnqualified | LibQualified
libAs   ::= LibNoAs | LibAs string

|| alias of import module names to local names
libAlias ::=
    Alias       name name |
    AliasRemove name

|| binding of %free variables or types 
libBinding ::=
    BindVar  name expr  |
    BindType name texpr

|| %import directive
libEnv == (fileSpec, location, libQual, libAs, [libBinding], [libAlias])

|| lenses on libEnv
libEnvFile  = lensTup6_0
libEnvLoc   = lensTup6_1
libEnvQual  = lensTup6_2
libEnvAs    = lensTup6_3
libEnvBind  = lensTup6_4
libEnvAlias = lensTup6_5

|| build status, used to differentiate modules that have only been parsed
|| with ones that are fully built (e.g. read in from a serialized version)
buildStatus ::= Unbuilt | Built | Serialized

|| Miranda top-level Library (Module) definition
module ::=
    Module
        fileSpec                   || module file
        buildStatus                
        (either [libPart] nameMap) || exports, initially a list of libParts, eventually reified into a nameMap
        [libEnv]                   || imports
        defnMap                    || top-level definitions (functions, simple patterns, data types, and type synonyms, all in the same namespace)
        [defn]                     || top-level patterns before they are desugared to a simple pattern
        defnMap                    || top-level type specifications
        nameMap                    || top-level environment for this module, mapping Unqualified names to Qualified names

showModule :: module -> string
showModule m
    = lay . map showDefn . m_keys $ defns
      where
        defns           = view modDefns m
        specs           = view modSpecs m
        showDefn n      = mshows (m_lookup cmpname n specs) . shows "\n" . mshows (m_lookup cmpname n defns) $ "\n"
        mshows Nothing  = shows ""
        mshows (Just a) = showsAst a

|| create an empty module with a name and initial environment
emptyModule :: fileSpec -> nameMap -> module
emptyModule fs env = Module fs Unbuilt (Left []) [] m_empty [] m_empty env

|| lenses for module components
modFile :: lens module fileSpec
modFile
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = fs
        overf fn (Module fs bs exs ins defs pats specs env) = Module (fn fs) bs exs ins defs pats specs env

|| the name of a module is implicitly its fileSpecName
modName :: lens module string
modName
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = fileSpecName fs
        overf fn (Module fs bs exs ins defs pats specs env) = error "cannot modify modName"

modBuildStatus :: lens module buildStatus
modBuildStatus
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = bs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs (fn bs) exs ins defs pats specs env

|| note: this lens works exclusively on the Left ([libPart]) form of exports
modExportParts :: lens module [libPart]
modExportParts
    = Lens getf overf
      where
        err = error "modExportParts: exports is not a parts list"

        getf (Module fs bs (Left exs) ins defs pats specs env) = exs
        getf  _ = err

        overf fn (Module fs bs (Left exs) ins defs pats specs env) = Module fs bs (Left (fn exs)) ins defs pats specs env
        overf fn _ = err

|| note: this lens works exclusively on the Right (nameMap) form of exports
|| also: the "set" Lens operation cannot be used with this lens to change a (Left [libPart]) form to a (Right nameMap) form; for
|| that, use setModExportMap
modExportMap :: lens module nameMap
modExportMap
    = Lens getf overf
      where
        err = error "modExportMap: exports not a map"

        getf (Module fs bs (Right exs) ins defs pats specs env) = exs
        getf _ = err                                                     

        overf fn (Module fs bs (Right exs) ins defs pats specs env) = Module fs bs (Right (fn exs)) ins defs pats specs env
        overf fn _ = err

|| used to replace the (Left [libPart]) form with a (Right nameMap) form, since a lens cannot effect the either change with an overf fn
setModExportMap :: nameMap -> module -> module
setModExportMap m (Module fs bs exs ins defs pats specs env) = Module fs bs (Right m) ins defs pats specs env

modImports :: lens module [libEnv]
modImports
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = ins
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs (fn ins) defs pats specs env

modImportFiles :: module -> [fileSpec]
modImportFiles = map (view libEnvFile) . view modImports

modDefns :: lens module defnMap
modDefns
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = defs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins (fn defs) pats specs env

modPats :: lens module [defn]
modPats
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = pats
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs (fn pats) specs env

modSpecs :: lens module defnMap
modSpecs
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = specs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs pats (fn specs) env

modEnv :: lens module nameMap
modEnv
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = env
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs pats specs (fn env)


|| defnMap operations

|| insert a name -> defn into a defnMap without checking for name clash or merging with a DefMult
insDefnMapUnchecked :: defn -> defnMap -> defnMap
insDefnMapUnchecked d m = m_insert cmpname (defnName d) d m

|| insert a Qualified name -> defn into a defnMap, or report a name clash
insDefnMap :: defn -> defnMap -> excpt defnMap
insDefnMap d m
    = ex_pure (m_insert cmpname n d m),        if isNothing md'
    = ex_error (NameClash n') (defnLocInfo d), otherwise
      where
        n   = defnName d
        md' = m_lookup cmpname n m
        d'  = fromJust md'
        n'  = defnName d'

|| insert a DefFn into a DefnMap by merging it into an existing DefMult, or create one
insDefMult :: defn -> defnMap -> excpt defnMap
insDefMult d m
    = ex_pure (m_insertWith cmpname addMult n dm m), if isNothing md' \/ isDefMult d'
    = ex_error (NameClash n') (defnLocInfo d),       otherwise
      where
        n              = defnName d
        md'            = m_lookup cmpname n m
        d'             = fromJust md'
        n'             = defnName d'
        dm             = DefMult Mfun [d]

        addMult (DefMult x1 defs') (DefMult x2 defs)
            = DefMult Mfun (defs ++ defs') || add new def after any existing defs to maintain pattern order

        addMult _  _ = error "insDefMult: not a DefMult"

|| insert a Qualified name -> defn into a defnMap, merging DefFns with a DefMult,
|| and merging abstract and implementation types
insDefnMapMerge :: defn -> defnMap -> excpt defnMap
insDefnMapMerge d m
    = insDefMult d  m,                  if isDefFn d
    = insDefAbs  d  d',                 if isDefAbs d & isJust md'      || abstract type defined before implementation synonym
    = insDefAbs  d' d,                  if isDefSyn d & isJust md'      || implementation synonym defined before abstract type
    = ex_pure (m_insert cmpname n d m), if isNothing md'
    = ncError,                          otherwise
      where
        n       = defnName d
        md'     = m_lookup cmpname n m
        d'      = fromJust md'
        ncError = ex_error (NameClash (defnName d')) (defnLocInfo d)

        || merge an implementation type synonym into a DefAbs
        insDefAbs (DefAbs tf ds t an) (DefSyn tf' t' an')
            = ex_error Arity (defnLocInfo d),                      if #tformVars tf ~= #tformVars tf'
            = ex_pure (m_insert cmpname n (DefAbs tf ds t' an) m), if _eq cmptexpr t builtinTypeType  || check for previous merging of implementation type

        insDefAbs d d' = ncError        || all other cases are a name clash

|| make a list of dependencies of each defn on the others in a defnMap, based upon the annotation's freeVar info
makeDefnDependencyList :: defnMap -> [(name, [name])]
makeDefnDependencyList defs
    = map (mapSnd (filter (converse (m_member cmpname) defs) . s_toList . view defnAnFree)) (m_toList defs)


|| functions to insert a top-level declaration into the correct part of a module, or report an error
modInserter == module -> excpt module

|| insert an %export declaration
modInsExp :: [libPart] -> modInserter
modInsExp pts m
    = ex_pure (over modExportParts (pts ++) m)

|| insert an %import declaration, qualifying any alias directives with the module name
modInsInc :: libEnv -> modInserter
modInsInc lenv
    = ex_pure . over modImports (lenv' :)
      where
        mn    = fileSpecName (view libEnvFile lenv)
        lenv' = over libEnvAlias (map doQual) lenv

        doQual (Alias n1 n2)   = Alias (qualifyName mn n1) (qualifyName mn n2)
        doQual (AliasRemove n) = AliasRemove (qualifyName mn n)


|| insert a %free declaration
modInsFre :: [defn] -> modInserter
modInsFre specs m = ex_error (Unsupported "%free") emptyLocInfo

|| insert a top-level definition
modInsDef :: defn -> modInserter
modInsDef d m
    = ex_fmap setDefs . insDefnMapMerge qd . view modDefns $ m
      where
        qd           = qualifyDefn (view modName m) d
        setDefs defs = set modDefns defs m

|| insert a top-level pattern
modInsPat :: defn -> modInserter
modInsPat d m
    = ex_pure (over modPats (qd :) m)
      where
        qd = qualifyDefn (view modName m) d

|| make a constructor that has strict fields by inserting an Ecase evaluation of those
|| fields before performing builtinPack
makeStrictCtor :: name -> int -> [texpr] -> anno -> defn
makeStrictCtor n tag cts an
    = DefFn n pvs (Elet False (m_singleton sc d) [] (foldr mkStrict (Ecall (Evar sc) evs) (zip2 evs cts))) an
      where
        sc        = (Internal "Sc" 0)           || name for internal constructor to call after fields made strict
        (_, vs )  = genNames "var" 0 cts        || generate names for the constructor vars
        pvs       = map Pvar vs
        evs       = map Evar vs
        d         = DefFn sc [] (builtinPack n tag (#cts)) an  || note: nullary DefFn instead of DefPat

        mkStrict (e, t) k
            = (Ecase Csingle e [CaseAlt Pwildcard k]), if isStrict t
            = k,                                       otherwise

|| insert a data definition:
|| insert a type specification for each constructor
|| insert a function for each constructor which is an alias to builtinPack
modInsData :: defn -> modInserter
modInsData d m
    = ex_foldM insFn m (enumerate ctors) >>=? modInsDef d
      where
        DefData dt ctors an = d

        insFn m (tag, (Tname n cts))
            = modInsSpec spec m >>=? modInsDef fn
              where
                t      = foldr Tarr dt $ map unStrict cts
                spec   = DefSpec (Tname n []) t an
                fn     = makeStrictCtor n tag cts an,              if any isStrict cts
                       = DefFn n [] (builtinPack n tag (#cts)) an, otherwise    || note: nullary DefFn instead of DefPat
        insFn m _      = undef  || added to remove compiler warning

|| insert a type specification
modInsSpec :: defn -> modInserter
modInsSpec d m
    = ex_fmap setSpecs . insDefnMap qd . view modSpecs $ m
      where
        qd             = qualifyDefn (view modName m) d
        setSpecs specs = set modSpecs specs m

|| insert an abstract type definition and its signature specs
modInsAbs :: defn -> modInserter
modInsAbs d m
    = ex_foldM (converse modInsSpec) m ds >>=? modInsDef d
      where
        DefAbs _ ds _ _ = d


|| make a module from an initial module and a sequence of modInserter functions
makeModule :: module -> [modInserter] -> excpt module
makeModule m insrs
    = ex_foldM (|>) m insrs'
      where
        mn     = view modName m
        stdlib = FileSpec stdlibPath stdlibModuleName
        stdInc = modInsInc (stdlib, initLocation mn, LibUnqualified, LibNoAs, [], [])

        insrs' = insrs,          if mn ==$ stdlibModuleName
               = stdInc : insrs, otherwise


|| global environment operations
|| the global environment is a state passed through the various module-modifying passes of the compiler; it
|| holds the current module being modified, a module map of all the modules in the build for cross-module
|| lookups for typechecking and inlining, and a counter sourcing ints for generating unique internal variable names
moduleMap == m_map string module        || map from module name to module
globalEnv == (module, moduleMap, int)   || global environment is current module, the moduleMap, and a generator number

|| lenses for globalEnv
genvMod :: lens globalEnv module
genvMod = lensTup3_0

genvModMap :: lens globalEnv moduleMap
genvModMap = lensTup3_1

genvGen :: lens globalEnv int
genvGen = lensTup3_2

|| update moduleMap with current module
updateEnv :: globalEnv -> globalEnv
updateEnv (m, mm, gen) = (m, m_insert cmpstring (view modName m) m mm, gen)

lookupModule :: globalEnv -> string -> maybe module
lookupModule genv n
    = Just m,                                      if n ==$ view modName m      || m is "cached version" of the module in mm; it may be more up-to-date
    = m_lookup cmpstring n (view genvModMap genv), otherwise
      where
        m  = view genvMod genv

lookupName :: globalEnv -> name -> maybe name
lookupName genv n
    = m_lookup cmpname n . view modEnv . view genvMod $ genv, if isUnqualified n \/ isUnresolved n
    = Just n,                                                 otherwise

|| lookup the definition of a Qualified name, using a lens to specify the defnMap to use
lookupDefnWithLens :: globalEnv -> lens module defnMap -> name -> maybe defn
lookupDefnWithLens genv l n
    = Nothing,                                  if isUnqualified n \/ isBuiltin n
    = lookupModule genv (getModName n) >>=
      (m_lookup cmpname n . view l),            otherwise

lookupDefn :: globalEnv -> name -> maybe defn
lookupDefn genv n
    = lookupName genv n >>= lookupDefnWithLens genv modDefns

|| lookup the type specification for a name in the global environment
|| tuple types are dynamically generated
lookupSpec :: globalEnv -> name -> maybe defn
lookupSpec genv n
    = lookupName genv n >>= getSpec
      where
        getSpec qn = Just (makeTupleSpec qn),             if isBuiltinTuple qn
                   = m_lookup cmpname qn builtinSpecs,    if isBuiltin qn
                   = lookupDefnWithLens genv modSpecs qn, otherwise

        makeTupleSpec qn = DefSpec (Tname qn []) (makeTupleCtorType qn) (genvAnno genv)

|| lookup an exportMap from a fileSpec
lookupExportMap :: locInfo -> globalEnv -> fileSpec -> excpt nameMap
lookupExportMap loci genv fs
    = fromMaybef err (ex_pure . view modExportMap) $ lookupModule genv mn
      where
        mn  = fileSpecName fs
        err = ex_error (Info $ "module " ++ mn ++ " not found") loci

|| lookup all the constructor names associated with a constructor name
|| generate name list for builtin constructors directly, since they aren't stored anywhere else
lookupCtorNames :: globalEnv -> name -> maybe [name]
lookupCtorNames genv n
    = Just [n],                                   if isBuiltinTuple n
    = Just [builtinNilName, builtinConsName],     if s ==$ getName builtinNilName \/ s ==$ getName builtinConsName
    = Just [builtinUnitName],                     if s ==$ getName builtinUnitName
    = lookupSpec genv n            >>=     || lookup spec for ctor, which should be a $Tarr b ... $Tarr (data type Tform)
      (lookupDefn genv . baseName) >>=     || lookup defn of data type
      (mb_pure . map tformName . view dataCtors), otherwise
      where
        s                            = getName n
        baseName (DefSpec tf te an ) = tformName $ baseType te
        baseName _                   = undef    || added to remove compiler warning

|| lookup the arity for a function reference in the global environment
|| since constuctor definitions use the poly-arity "pack" builtin, they don't have arguments,
|| so the arity for them is determined from their specs
lookupArity :: globalEnv -> name -> maybe int
lookupArity genv n
    = lookupSpec genv n >>= sa,  if isConstructor n \/ isBuiltin n
    = lookupDefn genv n >>= fca, otherwise
      where
        fca (DefFn n vars rhs an) = mb_pure $ #vars
        fca (DefPat p e an)       = mb_pure 0
        fca d                     = Nothing
        sa (DefSpec t te an)      = mb_pure $ typeArity te
        sa d                      = Nothing

|| create an anno for top-level defns from genv
genvAnno :: globalEnv -> anno
genvAnno = locAnno . initLocation . view modName . view genvMod


|| nameMap operations
|| note: these logically would be defined in the name module, but the operations here
|| can generate exceptions, which itself has a dependency upon the name module, so they
|| are defined here to remove the circularity

|| insert a mapping into a nameMap, or report a name clash
insNameMap :: locInfo -> name -> name -> nameMap -> excpt nameMap
insNameMap loc k v m
    = ex_pure (m_insert cmpname k v m), if isNothing mprev
    = ex_error (NameClash prev) loc,    otherwise
      where
        mprev = m_lookup cmpname k m
        prev  = fromJust mprev

|| insert a defn name into a nameMap, inserting all of the associated constructors if it is a data defn
insNameMapDefn :: nameMap -> name -> defn -> excpt nameMap
insNameMapDefn e n d
    = ex_foldM insDataName e ns, if isDefData d     || insert data type name and constructor names (implicitly Qualified)
    = insName e n (defnName d),  otherwise
      where
        loci = defnLocInfo d
        ns   = n : map tformName (view dataCtors d)

        insDataName e n = insName e n n

        insName e n n'
            = insNameMap loci (unresolveName n) n' e >>=?
              insNameMap loci (unqualifyName n) n'    || add both unresolved (qualified) and unqualified names

|| delete a name mapping from a nameMap, or report an undefined error
delNameMap :: locInfo -> name -> nameMap -> excpt nameMap
delNameMap loc k m
    = ex_pure (m_delete cmpname k m), if m_member cmpname k m \/ isUnqualified k        || delete or silently ignore, if Unqualified
    = ex_error (Undefined k) loc,     otherwise                                         || report an error, otherwise

|| replace a key name in the nameMap
|| used for aliasing names during import
replaceNameMapKey :: locInfo -> name -> name -> nameMap -> excpt nameMap
replaceNameMapKey loc k k' m
    = ex_pure m,                  if isNothing mprev & isUnqualified k || silently ignore replacement for unqualified names
    = delNameMap loc k m >>=?
      insNameMap loc k' prev,     otherwise
      where
        mprev = m_lookup cmpname k m
        prev  = fromJust mprev

|| merge nameMap e2 into nameMap e1, qualifying and renaming e2 names based upon libQual and libAs values
mergeEnvironments :: locInfo -> libQual -> libAs -> nameMap -> nameMap -> excpt nameMap
mergeEnvironments loc qual as e1 e2
    = ex_foldM (addName qual) e1 . map (renameAs as) . filter (isQualified . snd) . m_toList $ e2
      where
        renameAs LibNoAs   e      = e
        renameAs (LibAs s) (k, v)
            = (k, v),                        if isUnqualified k
            = (Unresolved s (getName k), v), otherwise

        addName LibUnqualified m (k, v) = insNameMap loc k v m
        addName LibQualified m (k, v)
            = ex_pure m,            if isUnqualified k
            = insNameMap loc k v m, otherwise

|| extract all of the top-level definitions from a specified module
definedInModule :: string -> nameMap -> nameMap
definedInModule s e
    = m_filter cmpname (fromModule s) e
      where
        fromModule s (Qualified mn v) = s ==$ mn
        fromModule s x                = False
