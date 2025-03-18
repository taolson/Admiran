|| rename.m -- AST renaming for qualifying names and making internal names unique


%export rename renameExpr

%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>                 (>>=)/st_bind
%import "ast"
%import "dependency"
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"


|| state passed through rename
|| dynamic state:
|| genv:  globalEnvironment
|| errs:  exceptions
|| prefix: string to prepend to renamed defns (used to mark internal and inlined functions for cleaner location reporting)
||
|| lexically-scoped state:
|| loc:  location info from enclosing definitions
|| env:  nameMap defining the lexical environment
|| tenv: set of tvars in scope
rnDynSt   == (globalEnv, [exception], string)
rnLexSt   == (locInfo, nameMap, s_set int)
rnSt      == (rnDynSt, rnLexSt)

|| lenses for rnSt fields
rnDyn    = lensFst
rnLex    = lensSnd
rnGenv   = composeLens rnDyn  lensTup3_0
rnErrs   = composeLens rnDyn  lensTup3_1
rnPrefix = composeLens rnDyn  lensTup3_2
rnLoc    = composeLens rnLex  lensTup3_0
rnEnv    = composeLens rnLex  lensTup3_1
rnTenv   = composeLens rnLex  lensTup3_2
rnGen    = composeLens rnGenv lensTup3_2

initRnSt :: globalEnv -> rnSt
initRnSt genv = ((genv, [], ""), (emptyLocInfo, view modEnv (view genvMod genv), s_empty))

|| add an exception to the rename state
addRnErr :: exceptionInfo -> rnSt -> rnSt
addRnErr ei st = over rnErrs ((Error, ei, view rnLoc st) :) st

|| add a rename to the rnEnv
addRnEnv :: name -> name -> rnSt -> rnSt
addRnEnv n1 n2 = over rnEnv $ m_insert cmpname n1 n2

|| lookup a name in the rnEnv
lookupRnEnv :: name -> rnSt -> maybe name
lookupRnEnv n = m_lookup cmpname n . view rnEnv

nameNeedsResolution :: name -> bool
nameNeedsResolution n
    = ~isWildcard n        &    || wildcard names are used for type holes during type checking; they can't be resolved
          (isUnqualified n \/
           isUnresolved  n \/
           isInternal    n)     || Internal names may still require resolution, due to renaming during inlining


renameAst :: astRewriteTraverser rnSt

|| rename an Elet by renaming its defnMap's keys, and adding them to the environment, then renaming the rest of the
|| Elet through the default continuation.  Also perform the optimization of removing any simple DefPat bindings
|| added during desugaring by changing them to a rename entries in the rnNameEnv
renameAst k (Elet rec defs x e) st
    = renameAst k e st2,          if m_null defs'                       || all defs were converted to rename entries
    = k (Elet rec defs' x e) st2, otherwise
      where
        (rdefs, ddefs) = partition isRenamePat $ m_toList defs
        g              = makeDependencyGraph cmpname $ map mkDep rdefs
        morder         = dependencyOrder cmpname g                      || rdefs must be handled in dependency-order
        ddefs'         = ddefs,          if isRight morder              || rdefs are linear
                       = rdefs ++ ddefs, otherwise                      || circular dependency detected; perform renameEntry for all defs
        (defs', st1)   = foldl renameEntry (m_empty, st) ddefs'         || install non-renamePat defns in the rename map first
        st2            = foldl renamePat st1 (fromEither [] morder)     || add renamePat defns as entries in the rename map

        isRenamePat (n, (DefPat (Pvar n1) (Evar n2) an)) = True
        isRenamePat x                                    = False

        mkDep (n, (DefPat (Pvar n1) (Evar n2) an)) = (n1, [n2])
        mkDep _ = undef || added to remove compiler warning                                    

        || handle renamePats by installing the mapping from n1 -> n2 in the rename map
        renamePat st n1
            = addRnErr (QualBind n1) st,   if isUnresolved n1
            = go (edges cmpname fwd n1 g), otherwise
              where
                go [n2] = addRnEnv n1 (fromMaybe n2 (lookupRnEnv n2 st)) st     || check if n2 is renamed already
                go x    = st    || n1 was not a renamePat; don't install a rename for it

        || rename other defns by running renameDef to get a new name, then install that in the defnMap
        || the rest of the defn will be renamed through the continuation
        renameEntry (defs, st) (n, d)
            = (m_insert cmpname n' d defs, st')
              where
                (n', st') = renameDef n st

|| rename an Efatbar by renaming the alternate leg in the current naming scope, then
|| creating a new unique name for its Pvar, then renaming the primary leg
|| with that new name in scope, to correctly rename any Efails in that leg
renameAst k (Efatbar (Pvar n) e1 e2) st
    = (Efatbar (Pvar n') e1' e2', st3)
      where
        (e2', st1) = astRewriteLex renameAst e2 st
        (n',  st2) = renameDef n st1
        (e1', st3) = astRewriteLex renameAst e1 st2

|| create new internal names for Unqualified name definitions and add them to the env, and
|| rename name references in an ast, or report an error if they are not found in the env
|| use the default continuation to rewrite sub-asts
renameAst k a st
    = (go a >>= k) st
      where
        go (CaseAlt p e)           = st_liftA2 CaseAlt    (renamePat p)    (st_pure e)
        go (Qgen ps e)             = st_liftA2 Qgen       (renamePats ps)  (st_pure e)
        go (Qrecur p e1 e2)        = st_liftA3 Qrecur     (renamePat p)    (st_pure e1)  (st_pure e2)
        go (Evar n)                = st_fmap   Evar       (renameRef n)
        go (Tname n tes)           = st_liftA2 Tname      (renameRef n)    (st_pure tes)
        go a                       = goDefn a . over rnLoc (mergeLocInfo (defnLocInfo a)), if isDefn a
                                   = st_pure a,                                            otherwise

        || The enclosing Elet has added renames for the defn names to the env, so to rename the defn itself, it's name
        || component is renamed as a ref, while any arg patterns are renamed as defs
        goDefn (DefFn n vars e an) = st_liftA4 DefFn   (renameRef n)    (renamePats vars) (st_pure e) (renameAnno an)
        goDefn (DefPat p e an)     = st_liftA3 DefPat  (renamePatRef p) (st_pure e)                   (renameAnno an)
        goDefn (DefSyn tf te an)   = st_liftA3 DefSyn  (tvarDefs tf)    (tvarRefs te)                 (renameAnno an)
        goDefn (DefData tf tes an) = st_liftA3 DefData (tvarDefs tf)    (st_mapM tvarRefs tes)        (renameAnno an)
        goDefn d                   = st_fmap (converse (set lensAnno) d) (renameAnno (view lensAnno d))

        renamePat (Pvar n)         = st_fmap Pvar $ renameDef n
        renamePat (Pctor n vars)   = st_liftA2 Pctor (renameRef n) (renamePats vars)
        renamePat x                = st_pure x
        renamePats                 = st_mapM renamePat

        renamePatRef (Pvar n)      = st_fmap Pvar $ renameRef n
        renamePatRef x             = st_pure x

        tvarDefs (Tname n tvs)     = st_fmap (Tname n) (st_mapM tvarDef tvs)
        tvarDefs _                 = undef      || added to remove compiler warning

        tvarRefs te                = astRewriteLex tvarRef te

        || rename the name references in the free set of an anno -- required because beta-reduction
        || during inlining calls renameExpr to rename the defn vars
        renameAnno an st
            = (set anFree free' an, st')
              where
                (free', st') = foldl renameEntry (s_empty, st) . s_toList $ view anFree an

                renameEntry (b, st) n
                    = (s_insert cmpname n' b, st')
                      where
                        (n', st') = renameRef n st

|| rename a name reference or report an undefinedName error
renameRef :: name -> state rnSt name
renameRef n st
    = fromMaybef (n, st') ($pair st) (lookupRnEnv n st), if nameNeedsResolution n
    = (n, st),                                           otherwise
      where
        st' = addRnErr (Undefined n) st

|| rename a name definition:
|| create a new Internal name if it is Unqualified, adding a prefix string if one is present
|| and add the mapping to the environment
renameDef :: name -> state rnSt name
renameDef n st
    = (n, addRnErr (QualBind n) st), if isUnresolved n  || definition names cannot be Unresolved (qualified with a module name) prior to rename
    = (n', st'),                     if nameNeedsResolution n
    = (n,  st),                      otherwise
      where
        pfx        = view rnPrefix st
        s          = getName n
        s'         = s,        if null pfx \/ isPrefixOf cmpchar pfx s
                   = pfx ++ s, otherwise
        (gen', n') = genName s' . view rnGen $ st
        st'        = addRnEnv n n' . set rnGen gen' $ st

|| check for unbound tvarReferences in a texpr 
tvarRef :: astRewriteTraverser rnSt
tvarRef k (Tvar n) st
    = (Tvar n, st')
      where
        st' = addRnErr (UnboundTvar n) st, if ~s_member cmpint n (view rnTenv st)
            = st,                          otherwise

tvarRef k te st = k te st

|| add a tvar definition to the environment, checking to see if it has already been defined
tvarDef :: texpr -> state rnSt texpr
tvarDef (Tvar n) st
    = (Tvar n, st')
      where
        st' = addRnErr (RepeatedTvar n) st,       if s_member cmpint n $ view rnTenv st
            = over rnTenv (s_insert cmpint n) st, otherwise

tvarDef te st = (te, st)


|| canonicalize Unqualified and Unresolved names in module definitions, by looking them up in the environment,
|| or creating new Internal names for names defined in Elets and patterns
rename :: globalEnv -> excpt globalEnv
rename genv
    = ex_return genv' warns, if null errs
    = ex_errs   errs,        otherwise
      where
        m             = view genvMod genv
        defs          = view modDefns m
        specs         = view modSpecs m
        (defs',  st1) = astRewriteMap (astRewriteLex renameAst) defs (initRnSt genv)     || top-level defs and specs are already qualified,
        (specs', st2) = astRewriteMap (astRewriteLex renameAst) specs st1                || so no need for "astRewriteInLet"
        genv'         = over genvMod (set modDefns defs' . set modSpecs specs') $ view rnGenv st2
        (errs, warns) = partition isError $ view rnErrs st2

|| rename a single expr, used in inlining
renameExpr :: string -> expr -> globalEnv -> (expr, globalEnv)
renameExpr pfx e genv
    = (e', view rnGenv st')
      where
        st        = set rnPrefix pfx $ initRnSt genv
        (e', st') = astRewriteLex renameAst e st
