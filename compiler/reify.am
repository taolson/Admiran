|| reify.m -- module environment reification of includes, exports, and expansion of type synonyms


%export reifyImports reifyDefns reifyDefAbs reifyExports reifySyns reifyTypes

%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "ast"
%import "dependency"
%import "exception"             (>>=)/ex_bind (<$>)/ex_fmap
%import "grammar"
%import "module"
%import "name"


|| add imported environments to the top-level environment, processing aliases and removes
reifyImports :: globalEnv -> excpt globalEnv
reifyImports genv
    = updateGenv <$> ex_foldM reifyImport m (view modImports m)
      where
        m   = view genvMod genv

        updateGenv m = set genvMod m genv

        reifyImport m (fs, loc, qual, as, binds, aliases)
            = updateModEnv <$> (getExports >>= doAliases >>= doMerge), if null binds
            = ex_error (Unsupported "bindings in %import") loci,       otherwise
              where
                loci = locOnlyInfo loc
                env  = view modEnv m

                updateModEnv e = set modEnv e m 
                getExports     = lookupExportMap loci genv fs
                doAliases ienv = ex_foldM aliasNameMap ienv aliases
                doMerge        = mergeEnvironments loci qual as env

                aliasNameMap e (AliasRemove n)
                    = delNameMap loci (unresolveName n) e >>=
                      delNameMap loci (unqualifyName n)

                aliasNameMap e (Alias n' n)
                    = replaceNameMapKey loci (unresolveName n) (unresolveName n') e >>=
                      replaceNameMapKey loci (unqualifyName n) (unqualifyName n')

|| reify module definitions that match a qualifying function
reifyDefns :: (defn -> bool) -> globalEnv -> excpt globalEnv
reifyDefns defType genv
    = updateGenv <$> ex_foldM ins env defLst
      where
        m      = view genvMod genv
        env    = view modEnv m
        defLst = filter (defType . snd) . m_toList . view modDefns $ m

        updateGenv e = set genvMod (set modEnv e m) genv
        ins e (n, d) = insNameMapDefn e n d

|| convert unreified list of export directive parts to a reified export environment
reifyExports :: globalEnv -> excpt globalEnv
reifyExports genv
    = updateGenv <$> doExports exps
      where
        m    = view genvMod genv
        mn   = view modName m
        exps = view modExportParts m
        env  = view modEnv m

        updateGenv m       = set genvMod m genv 
        updateExportMap nm = setModExportMap nm m

        doExports []   = doExports [LibAll (initLocation mn)]           || default is to export everything
        doExports exps = updateExportMap <$> ex_foldM doPart m_empty exps

        doPart e (LibAll loc)    = doMerge loci e (definedInModule mn env)        where loci = locOnlyInfo loc
        doPart e (LibFile f loc) = lookupExportMap loci genv f >>= doMerge loci e where loci = locOnlyInfo loc

        doPart e (LibRemove n loc)
            = delNameMap loci (unresolveName n') e >>=
              delNameMap loci (unqualifyName n')
              where
                loci = locOnlyInfo loc
                n'   = qualifyName mn n, if isUnqualified n
                     = n,                otherwise

        doPart e (LibIdent n loc)
            = ex_error (Undefined n) loci,       if isNothing md
            = insNameMapDefn e qn (fromJust md), otherwise
              where
                loci = locOnlyInfo loc
                qn   = qualifyName mn n
                md   = m_lookup cmpname n env $mb_bind lookupDefn genv

        doMerge loci e1 e2 = mergeEnvironments loci LibUnqualified LibNoAs e1 e2


|| reification (expansion) of type synonyms

|| state passed through expansion
tsSt == ((globalEnv, [exception]), locInfo)

|| lenses for tsSt fields
tsGenv     = composeLens lensFst lensFst
tsErrs     = composeLens lensFst lensSnd
tsLoc      = lensSnd

genvModDefns = genvMod $composeLens modDefns
genvModSpecs = genvMod $composeLens modSpecs
tsModDefns   = tsGenv  $composeLens genvModDefns
tsModSpecs   = tsGenv  $composeLens genvModSpecs

initTsSt :: globalEnv -> tsSt
initTsSt genv = ((genv, []), emptyLocInfo)

addTsErr :: exceptionInfo -> locInfo -> tsSt -> tsSt
addTsErr ei loc = over tsErrs ((Error, ei, loc) :)


|| expand references to type synonyms in a texpr
expand :: astRewriter tsSt
expand (Tname n vars) st
    = (Tname n vars, st),        if isBuiltin n \/ isConstructor n \/ isNothing mdef
    = (Tname n vars, stNotType), if ~isDefType def
    = (Tname n vars, stArity),   if #vars ~= #vars'       || add an error to st if arities don't match
    = (renamedTe, st),           if isSyn def
    = (Tname n vars, st),        otherwise
      where
        loc            = view tsLoc st
        mdef           = lookupDefn (view tsGenv st) n
        def            = fromJust mdef
        Tname _ vars'  = view defnTform def
        te'            = view defnType def
        rnMap          = m_fromList cmptexpr (zip2 vars' vars)
        (renamedTe, _) = astRewriteTDCont renameTvars te' rnMap   || note: TDCont used to prevent infinite loop in renaming Tvars

        stNotType      = addTsErr (NotType n) loc st
        stArity        = addTsErr Arity loc st

        renameTvars k (Tvar n) rnMap = (m_findWithDefault cmptexpr (Tvar n) (Tvar n) rnMap, rnMap)
        renameTvars k a        s     = k a s

expand a s = (a, s)


|| expand a type synonym to a base type
expandSyn :: tsSt -> name -> tsSt
expandSyn st n
    = over tsModDefns (m_insert cmpname n d') st'
      where
        genv       = view tsGenv st
        d          = fromJust (lookupDefn genv n)        || guaranteed to exist
        te         = view defnType d
        loc        = defnLocInfo d
        (te', st') = astRewriteTD expand te (set tsLoc loc st)
        d'         = set defnType te' d

|| expand type synonyms in dependency order
expandSyns :: tsSt -> tsSt
expandSyns st
    = addTsErr (DepCycle (showCycles cycs)) emptyLocInfo st, if isLeft morder
    = foldl expandSyn st order,                              otherwise
      where
        synDefs     = m_filter cmpname isSyn (view tsModDefns st)
        morder      = dependencyOrder cmpname . makeDependencyGraph cmpname . makeDefnDependencyList $ synDefs
        Left cycs   = morder
        Right order = morder

        showCycles cs
            = shows "type definitions: " . interShows ", " (map showsCycle cs) $ ""
              where
              showsCycle ns = showsInParens (interShows ", " (map showsName ns))

|| expand type synonyms used in defSpecs
expandSpecs :: [defn] -> tsSt -> tsSt
expandSpecs specs st
    = foldl expandSpec st specs
      where
        expandSpec st d

            = over tsModSpecs (insDefnMapUnchecked d') st'
              where
                loc        = defnLocInfo d
                (te', st') = astRewriteTD expand (view defnType d) (set tsLoc loc st)
                d'         = set defnType te' d

|| expand type synonyms used in data defns
expandDataDefs :: [defn] -> tsSt -> tsSt
expandDataDefs ddefs st
    = foldl expandData st ddefs
      where
        expandData st d
            = over tsModDefns (insDefnMapUnchecked d') st'
              where
                loc           = defnLocInfo d
                (ctors', st') = astRewriteList (astRewriteTD expand) (view dataCtors d) (set tsLoc loc st)
                d'            = set dataCtors ctors' d

|| reify type synonyms, checking for dependency loops
reifySyns :: globalEnv -> excpt globalEnv
reifySyns genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        st            = expandSyns (initTsSt genv)
        (errs, warns) = partition isError (view tsErrs st)

|| reify specs and data defs
reifyTypes :: globalEnv -> excpt globalEnv
reifyTypes genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        specs         = filter isDefSpec . m_elems . view genvModSpecs $ genv
        ddefs         = filter isDefData . m_elems . view genvModDefns $ genv
        st            = expandDataDefs ddefs . expandSpecs specs . initTsSt $ genv
        (errs, warns) = partition isError (view tsErrs st)

|| reify module abstype definitions by converting them to opaque DefData with no construtors,
|| then re-expanding their signature specs
reifyDefAbs :: globalEnv -> excpt globalEnv
reifyDefAbs genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        absTys        = filter isDefAbs . m_elems . view genvModDefns $ genv
        st            = foldl update (initTsSt genv) absTys
        (errs, warns) = partition isError (view tsErrs st)

        update st (DefAbs tf specs te an)
            = expandSpecs specs . over tsModDefns (insDefnMapUnchecked d) $ st
              where
                d = DefData tf [] an

        update _ _ = undef      || added to remove compiler warning
