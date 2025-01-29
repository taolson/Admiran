|| demand.m -- perform demand analysis on an AST, annotating DefFns with the strictness info of their vars,
||             and marking thunks which are demanded to NoUpdate (anStrict == s_singleton (-1))
||
|| DEBUG: the setting of [-1] for demanded, single-use DefPats has problems:
|| 1) they sometimes disappear (show as [] in final output), but haven't diagnosed why some disappear and others dont
|| 2) is wrong anyway, because a single-use DefPat could escape, anyway, allowing them to be evaluated multiple times


%export demand

%import <either>
%import <lens>
%import <map>
%import <maybe>                 (<$>?)/mb_fmap (<|>?)/mb_alt
%import <mirandaExtensions>
%import <set>
%import <state>                 (>>=)/st_bind (<$>)/st_fmap (<<)/st_left (>>)/st_right
%import "ast"
%import "config"
%import "dependency"
%import "exception"
%import "grammar"
%import "name"
%import "module"


|| state passed through demand:
|| genv:     globalEnv, for looking up fn demand info in other modules
|| dmMap:    map from name to its demand set for this module
|| dmNames:  in-scope demand variable names
|| notes:    information notes collected during traversal

demandSet == s_set num
dmSt      == (globalEnv, m_map name demandSet, s_set name, [exception])

|| lenses for dmSt
dmGenv  = lensTup4_0
dmMap   = lensTup4_1
dmNames = lensTup4_2
dmNotes = lensTup4_3

initDmSt :: globalEnv -> dmSt
initDmSt genv = (genv, m_empty, s_empty, [])

|| lens for accessing the strict field of a defn anno
defnAnnoStrict = lensAnno $composeLens anStrict

|| lift lens operations into dmSt
dm_view :: lens dmSt * -> state dmSt *
dm_view lns st = (view lns st, st)

dm_set :: lens dmSt * -> * -> state dmSt ()
dm_set lns v st = ((), set lns v st)

dm_over :: lens dmSt * -> (* -> *) -> state dmSt ()
dm_over lns f st = ((), over lns f st)


|| add a name to the demanded names
addDmName :: name -> state dmSt ()
addDmName n = dm_over dmNames (s_insert cmpname n)

|| delete a list of names from the dmNames and dmMap (to remove variable names after they go out of scope)
|| note that this can be done by deleting keys, since renaming has guaranteed that all names are
|| now unique, so we can keep them in a single set, rather than lexically scope them
delDmNames :: [name] -> state dmSt ()
delDmNames ns
    = dm_over dmNames (converse (foldr (s_delete cmpname)) ns) >>
      dm_over dmMap   (converse (foldr (m_delete cmpname)) ns)

|| add a note to the demand state, limiting the length of the note string
addNote :: string -> state dmSt ()
addNote s
    = dm_over dmNotes ((Note, Info s', emptyLocInfo) :), if demandVerbose
    = st_pure (),                                        otherwise
      where
        s' = limitInfo 100 s

|| get the demandSet for a function from the local dsMap or from its anno in the globalEnv
lookupDemand :: name -> state dmSt demandSet
lookupDemand n
    = st_liftA2 go (dm_view dmGenv) (dm_view dmMap)
      where
        go genv dm
            = s_empty,                             if isConstructor n        || constructors are WHNF and don't demand their arguments
            = s_fromList cmpint  [0 .. arity - 1], if isBuiltin n            || builtins demand all of their arguments
            = fromMaybe s_empty mds,               otherwise
              where
                arity = fromJust (lookupArity genv n)
                mds   = m_lookup cmpname n dm <|>? view defnAnnoStrict <$>? lookupDefn genv n


|| traverse the AST, collecting the Evar names that are encountered on the strict evaluation path
|| annotate defFns with the discovered demand of their vars, and mark any DefPats that are demanded or
|| are simple Constructor calls as NoUpdate
demandAst :: astRewriteTraverser dmSt

demandAst k (Elet rec defs pats e)
    = (dm_view dmNames >>=
       doDefs          >>=
       doExpr          >>=
       mkStrictPat)    <<
      delDmNames (m_keys defs)
      where
        || find the demand for the defFns in the defs, forgetting any discovered dmNames from the defns
        || since the defs aren't on the strict evaluation path
        doDefs dnsIn = astRewriteMap (astRewriteTDCont demandAst) defs << dm_set dmNames dnsIn

        || find the demand for the expr (on the strict evaluation path)
        doExpr defs' = pair defs' <$> astRewriteTDCont demandAst e

        || otherwise, return the Elet with the newly-annotated defs and expr, marking
        || DefPats that are demanded single-use or are simple Ctor calls with a [-1] in their strictness anno field
        mkStrictPat (defs', e')
            = dm_view dmNames >>= checkPat
              where
                (dn, d)  = m_first defs'         || the rewrite pass has made all non-recursive Elets singletons, so we only need to check the first
                loc      = view defnAnLoc d
                uses     = view defnAnUses d
                d'       = set defnAnStrict (s_singleton (-1)) d

                isCtorCall (Ecall (Evar n) args) = isConstructor n
                isCtorCall a                     = False

                checkPat dns
                    = st_pure noUpdLet << addNote note, if noUpd
                    = st_pure updLet,                   otherwise
                      where
                        noUpd    = ~rec & isDefPat d & (isCtorCall e' \/ uses < 2 & s_member cmpname dn dns)
                        updLet   = Elet rec (m_singleton dn d)  pats e'
                        noUpdLet = Elet rec (m_singleton dn d') pats e'
                        note     = showName dn ++ " marked NoUpdate "  ++ showsLocation loc ""

|| an Ecase will traverse the scrutinee on the strict evaluation path, then ONE of the alts.  Since we don't
|| know which alt at compile time, the best we can do is return the dmNames that occur in ALL paths, so
|| compute them separately, then combine with a set intersection
demandAst k (Ecase sel e alts)
    = st_liftA2 (Ecase sel) (astRewriteTDCont demandAst e) (dm_view dmNames >>= doAlts)
      where
        || compute each alt separately, then combine the separately-discovered dmNames
        doAlts dns = st_mapM (doAlt dns) alts >>= (combineAlts . unzip2)

        || peform demand analysis on the alt, then return it, along with its dmNames as a pair
        doAlt dns alt
            = st_liftA2 pair (dm_set dmNames dns >> astRewriteTDCont demandAst alt) (dm_view dmNames)

        || combine all the alt's dmNames with a set intersection
        combineAlts (alts', dnss) = st_pure alts << dm_set dmNames (foldl1 (s_intersect cmpname) dnss )

|| check if any args are demanded by a fn call, and add them to the dmNames
|| also add the function name to the list, as it could be a thunk
demandAst k (Ecall e args)
    = doFn   >>=
      doArgs >>=
      checkFn
      where
        doFn            = st_liftA2 pair (astRewriteTDCont demandAst e) (dm_view dmNames)
        doArgs (e, dns) = pair e <$> astRewriteList (astRewriteTDCont demandAst) args << dm_set dmNames dns

        checkFn (Evar f, args)
            = st_pure (Ecall (Evar f) args)         <<
              dm_over dmNames (s_insert cmpname f)  <<
              (lookupDemand f >>= checkArgs)

        checkFn (e, args)       = st_pure (Ecall e args)
        checkArgs ds            = st_mapM_ (checkArg ds) (enumerate args)

        checkArg ds (i, Evar n) = dm_over dmNames (s_insert cmpname n), if s_member cmpint i ds
        checkArg ds x           = st_pure ()

|| Efatbar needs to go down the left path only for demand
demandAst k (Efatbar p e1 e2)
    = st_liftA2 (Efatbar p) (astRewriteTDCont demandAst e1) (dm_view dmNames >>= doAlt) << delDmNames (patNames p)
      where
        doAlt dns = astRewriteTDCont demandAst e2 << dm_set dmNames dns

|| delete CaseAlt patNames after discovering the demand of its expr
demandAst k (CaseAlt p e) = k (CaseAlt p e) << delDmNames (patNames p)

|| add an Evar reference (thunk evaluation) to the dmNames
demandAst k (Evar n) = st_pure (Evar n) << addDmName n

demandAst k a
    = st_bind2 (k a) (dm_view dmNames) doDef, if isDefFn a
    = k a,                                    otherwise
      where
        doDef (DefFn n vars e an) dns
            = dm_over dmMap (m_insert cmpname n strict) >>     || add strictness info for n into dmMap
              delDmNames vns                            >>     || remove now out-of-scope var names from dmNames
              st_pure (DefFn n vars e an')
              where
                vns    = map unPvar vars
                strict = foldl findVarIdx s_empty (enumerate vns)
                an'    = set anStrict strict an

               findVarIdx s (i, n)
                   = s_insert cmpint i s, if s_member cmpname n dns 
                   = s,                   otherwise

       doDef _ _ = undef  || added to remove compiler warning

demandGroup :: defnMap -> [name] -> state dmSt defnMap
demandGroup defs ns
    = dm_view dmGenv >>= doGroup
      where
        doGroup genv
            = addDefs <$> astRewriteList (astRewriteTDCont demandAst) defs'
              where
                defs'        = catMaybes (map (lookupDefn genv) ns)
                addDefs      = foldl ins defs . zip2 ns
                ins m (k, v) = m_insert cmpname k v m

demand :: globalEnv -> excpt globalEnv
demand genv
    = ex_return genv  [],    if ~useDemandAnalysis
    = ex_return genv' warns, if null errs
    = ex_errs errs,          otherwise
      where
        m             = view genvMod genv
        sccs          = (findSCCs cmpname . makeDependencyGraph cmpname . makeDefnDependencyList . view modDefns) m
        (defs', st)   = st_foldM demandGroup m_empty sccs (initDmSt genv)
        genv'         = over genvMod (set modDefns defs') (view dmGenv st)
        (errs, warns) = partition isError (view dmNotes st)
