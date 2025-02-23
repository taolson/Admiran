|| typecheck.m -- Hindly-Milner type checker (with Milner-Mycroft typability allowing polymorphic recursion)
||
|| Milner-Mycroft polymorphic recursion allows the definition of datatypes like finger trees where a type parameter
|| can change with each recursive invocation (no monomorphic restriction)
||
|| version that uses substitution maps instead of compositions of substitution functions (O(logN) instead of O(N))
|| and applicative functors on tcSt
||
|| to do: add check for trying pass an unboxed word# to a polymorphic function, or storing one in a polymorphic data type
||        should check for trying to bind a word# in a let be moved to the let/letrec, instead of in defPat?


%export typecheck

%import <bag>
%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <maybeState>            (>>=)/mst_bind tc_pure/mst_pure tc_fail/mst_fail tc_maybe/mst_maybe (<$>)/mst_fmap
                                tc_liftA2/mst_liftA2 tc_bind2/mst_bind2  (<*)/mst_left (*>)/mst_right
                                tc_mapM/mst_mapM tc_foldM/mst_foldM
%import <set>
%import <state>
%import "ast"
%import "dependency"
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"


|| a substMap is a map from a Tvar number to a texpr which substitutes for it
tvSet    == s_set int
substMap == m_map int texpr

id_subst :: substMap
id_subst = m_empty

|| apply a substitution to a Tvar number
subst :: substMap -> int -> texpr
subst sm n = m_findWithDefault cmpint (Tvar n) n sm

|| apply a substitution to a texpr
t_subst :: substMap -> texpr -> texpr
t_subst sm t
    = fst $ astRewriteTD subt t sm
      where
        subt (Tvar n) sm = (subst sm n, sm)
        subt t        sm = (t, sm)

|| find the free Tvars in a texpr
t_ftv :: texpr -> tvSet
t_ftv t
    = astAccumTD ftv t s_empty
      where
        ftv (Tvar v) s = s_insert cmpint v s
        ftv a        s = s

|| compose substitutions
subComp :: substMap -> substMap -> substMap
subComp sm sm' = m_union cmpint sm $ m_fmap (t_subst sm) sm'

|| normalize the Tvars used in a texpr to the lowest unique values
normalize :: texpr -> texpr
normalize t
    = fst $ astRewriteTD norm t (m_empty, 1)
      where
        norm (Tvar n) (m, gen)
            = (Tvar gen, (m_insert cmpint n gen m, gen + 1)), if isNothing mn'
            = (Tvar n', (m, gen)),                            otherwise
              where
                mn' = m_lookup cmpint n m
                n'  = fromJust mn'
        norm t s = (t, s)


|| a type scheme has a set of schematic variables which may be freely instantiated to
|| conform with the type constraints on the various occurrences of that variable.
|| all other (non-schematic) variables in a type scheme are constrained, and must not
|| be instantiated when instantiating the type scheme
scheme ::= Forall tvSet texpr

|| make a scheme from a DefSpec or texpr
mkScheme :: texpr -> scheme
mkScheme (DefSpec tf te an) = Forall (t_ftv te) te
mkScheme te                 = Forall (t_ftv te) te

|| extract the texpr from a scheme
unScheme :: scheme -> texpr
unScheme (Forall ts te) = te

|| apply a substitution to a scheme
s_subst :: substMap -> scheme -> scheme
s_subst sm (Forall as t)
    = Forall as $ t_subst sm' t
      where
        sm' = foldr (m_delete cmpint) sm (s_toList as)  || remove the schematic variables from the substitution

|| find the free Tvars in a scheme
s_ftv :: scheme -> tvSet
s_ftv (Forall as t) = s_difference cmpint (t_ftv t) as


|| schemeMap operations

typeMap   == m_map name texpr
schemeMap == m_map name scheme

|| perform a substitution on a schemeMap
sm_subst :: substMap -> schemeMap -> schemeMap
sm_subst sm = m_fmap $ s_subst sm

|| find free tvars in a schemeMap
sm_ftv :: schemeMap -> tvSet
sm_ftv = foldl (s_union cmpint) s_empty . map s_ftv . m_elems


|| typeEnv operations

typeEnv == [schemeMap]  || using a lexical stack of schemeMaps, instead of merging into one large schemeMap, is faster overall

|| apply a substitution to a type environment
te_subst :: substMap -> typeEnv -> typeEnv
te_subst sm = map $ sm_subst sm

|| find free Tvars in a type environment
te_ftv :: typeEnv -> tvSet
te_ftv = foldl (s_union cmpint) s_empty . map sm_ftv


|| state passed through typecheck
|| dynamic state:
|| genv:    global environment
|| schemes: schemes dynamically created from referenced type specs
|| fvars:   free variables in top level texpr of a subsume
|| errs:    exceptions collected during traversal
||
|| lexically-scoped state:
|| loc: location info from enclosing definition
|| env: lexical type environment
tcDynSt == (globalEnv, schemeMap, tvSet, [exception])
tcLexSt == (locInfo, typeEnv)
tcSt    == (tcDynSt, tcLexSt)

|| lenses for tcSt fields
tcDyn   = lensFst
tcLex   = lensSnd
tcGenv  = composeLens tcDyn  lensTup4_0
tcScms  = composeLens tcDyn  lensTup4_1
tcFvars = composeLens tcDyn  lensTup4_2
tcErrs  = composeLens tcDyn  lensTup4_3
tcLoc   = composeLens tcLex  lensFst
tcEnv   = composeLens tcLex  lensSnd
tcGen   = composeLens tcGenv lensTup3_2
tcSpecs = tcGenv $composeLens genvMod $composeLens modSpecs

initTcSt :: globalEnv -> tcSt
initTcSt genv = ((genv, m_empty, s_empty, []), (emptyLocInfo, []))

addTcWarn, addTcErr, addTcNote :: exceptionInfo -> locInfo -> tcSt -> tcSt
addTcWarn ei loc = over tcErrs ((Warn,  ei, loc) :)
addTcErr  ei loc = over tcErrs ((Error, ei, loc) :)

|| attach additional info to the most recent exception, or create a new Note
addTcNote info loc
    = over tcErrs attach
      where
        attach []                          = [(Note, info, loc)]
        attach ((et, ExMult eis, el) : es) = (et, ExMult (eis ++ [info]), el) : es
        attach ((et, ei, el) : es)         = (et, ExMult [ei, info], el) : es


tcState * == maybeState tcSt *

|| alternative interface
|| note: this cannot simply be an alias to mst_alt, because we need to keep the exceptions generated
|| in the primary leg and pass them to the secondary leg
(<|>) :: tcState * -> tcState * -> tcState *
ma <|> mb
    = go
      where
        go s = alt $ mst_runState ma s
               where
                 alt (Nothing, s') = mst_runState mb $ set tcErrs (view tcErrs s') s
                 alt ra            = ra

|| add an error to the state and fail with Nothing
tc_error, tc_errorNote :: exceptionInfo -> tcState *
tc_error ei st     = (Nothing, addTcErr  ei (view tcLoc st) st)
tc_errorNote ei st = (Nothing, addTcNote ei (view tcLoc st) st)

|| lift lens operations into tcState
tc_view :: lens tcSt * -> tcState *
tc_view lns st = (Just (view lns st), st)

tc_set :: lens tcSt * -> * -> tcState ()
tc_set lns v st = (Just (), set lns v st)

tc_over :: lens tcSt * -> (* -> *) -> tcState ()
tc_over lns f st = (Just (), over lns f st)


|| lookup a scheme in the current environment
lookupEnv :: name -> typeEnv -> maybe scheme
lookupEnv n = foldr (mb_alt . m_lookup cmpname n) Nothing

|| lookup (or dynamically create) the type scheme for a name in the global environment
|| note: caching schemes in tcScms doesn't appear to speed things up (3% slower in compiling advent2021 suite) -- remove to make simpler?
lookupScheme :: name -> tcState scheme
lookupScheme n st
    = tc_maybe msl st,           if isJust msl
    = tc_maybe msd st,           if isJust msd
    = tc_pure  scm st',          if isJust msp
    = tc_error (Undefined n) st, if isNothing mtd & isNothing mtb
    = tc_error (TypeIdent n) st, if isJust mtd & (isDefSyn def \/ isDefData def \/ isDefAbs def) \/ isJust mtb
    = tc_error (WrongType n) st, otherwise      || inferred spec for an un-spec'd defn was not installed prior
      where
        genv = view tcGenv st
        msl  = lookupEnv n (view tcEnv st)              || search current lexical environment
        msd  = m_lookup cmpname  n (view tcScms st)     || search global schemes
        msp  = lookupSpec  genv n                       || lookup type spec in global environment
        scm  = mkScheme  (fromJust msp)                 || and install in the global schemes, if found
        mtd  = lookupDefn genv n                        || lookup defn in global environment
        def  = fromJust mtd
        mtb  = m_lookup cmpname (unqualifyName n) builtinEnv
        st'  = over tcScms (m_insert cmpname n scm) st


|| type inference

|| generate a new Tvar number from the state
newTvar :: tcState texpr
newTvar = Tvar <$> (tc_view tcGen) <* tc_over tcGen (+ 1)

|| create a new instance of a type scheme by creating new Tvars for the schematic vars
instantiate :: scheme -> tcState (texpr, substMap)
instantiate (Forall as t)
    = tc_mapM (const newTvar) asl >>= mkSm
      where
        asl = s_toList as
        mkSm asl'
            = tc_pure (t_subst sm t, id_subst)
              where
                sm = m_fromList cmpint $ zip2 asl asl'

|| generalize a texpr to a scheme
generalize :: typeEnv -> texpr -> scheme
generalize env t
    = Forall as t
      where
        as = s_difference cmpint (t_ftv t) (te_ftv env)


|| perform an inference in a new lexical environment, restoring the environment afterwards
|| note: inference may result in an exception, so explicitly manipulate the state rather than
|| use tc_ applicatives so that we guarantee the lex state is restored
inLexEnv :: schemeMap -> (* -> tcState **) -> * -> tcState **
inLexEnv env action e st
    = (r, set tcLex lex st')
      where
        lex      = view tcLex st
        (r, st') = action e . over tcEnv (env :) $ st

|| capture and restore the lexical state around an inference
lexMono, lexPoly :: typeMap -> (* -> tcState **) -> * -> tcState **

|| lexMono uses monomorphic bindings for the new environment frame
lexMono tmap inf e
    = inLexEnv smap inf e
      where
        smap = m_fmap (Forall s_empty) tmap

|| lexPoly uses polymorphic bindings for the new environment frame
lexPoly tmap inf e
    = tc_view tcEnv >>= f
      where
        f env
            = inLexEnv smap inf e
              where
                smap = m_fmap (generalize env) tmap


|| generate a typeMap of new Tvars from a defnMap
genTypeMap :: defnMap -> tcState typeMap
genTypeMap dm
    = mkMap <$> tc_mapM (const newTvar) ds
      where
        (ks, ds) = unzip2 $ m_toList dm
        mkMap    = m_fromList cmpname . zip2 ks

|| make a typeMap from a list of patterns and corresponding texprs
|| filter out any non-Pvar (wildcard) patterns from the typeMap
mkTypeMap :: [pat] -> [texpr] -> typeMap
mkTypeMap ps ts
    = m_fromList cmpname . map (mapFst unPvar) . filter (isPvar . fst) $ zip2 ps ts

|| report extra info when an error is encountered in mutual recursive definitions
mutRecErr :: [name] -> tcState *
mutRecErr ns
    = tc_fail,           if #ns < 2       || don't add additional note if just a single (self-recursive) name
    = tc_errorNote info, otherwise
      where
        s    = intercalate ", " $ map (converse showsExName "") ns
        info = Info $ "in mutually-recursive definition of " ++ s


|| infer the type of an expr and return the inferred type and a substitution map, or report an error
|| note: infer performs an AST traversal, but we can't use the traversals provided in ast, because
|| typecheck uses a maybeState instead of just state.  Also, we aren't rewriting the ast, but we are
|| returning a value (rather than just the state value from accum), and we have to explicitly write
|| an infer for each different AST node, anyway.  So a custom traversal scheme is used, here
infer :: expr -> tcState (texpr, substMap)
infer (Eint n)        = tc_pure (stdInt, id_subst)
infer (Echar c)       = tc_pure (stdChar, id_subst)
infer (Estring s)     = tc_pure (Tlist stdChar, id_subst)
infer (EprimInt n)    = tc_pure (builtinTypeWord, id_subst)
infer (EprimChar c)   = tc_pure (builtinTypeWord, id_subst)
infer (EprimString s) = tc_pure (builtinTypeWord, id_subst)
infer (Evar n)
    = tc_pure ((Tname n []), id_subst), if isWildcard n || wildcards used as type holes
    = lookupScheme n >>= instantiate,   otherwise

|| handle polymorphic pseudo-functions pack and sel
|| the constructor name to use for the function is passed as the first argument
infer (Tname n vars)
    = tc_bind2 (tc_view tcEnv) (lookupScheme cn) f      || lookup the scheme for the constructor
      where
        (Tname cn []) = vars ! 0                        || constructor passed as first argument
        (EprimInt i)  = vars ! 1                        || index passed as second argument
        f env s
            = instantiate s'
              where
                (tl, bt) = typeList . unScheme $ s    || the type components of the Ctor type (separated by Tarr)
                nt       = tl ! i                     || the type of the nth constructor argument
                s'       = generalize env (bt $Tarr nt), if _eq cmpname n builtinSelName
                         = s,                            otherwise

infer (Eap e1 e2)
    = tc_bind2 newTvar (inferList [e1, e2]) f1
      where
        f1 tv ([t1, t2], sm) = unify t1 (t2 $Tarr tv) sm >>= f2 tv         || t1 == t2 -> *
        f1 _  _              = undef    || added to remove compiler warning

        f2 tv sm             = tc_pure (t_subst sm tv, sm)

infer (Ecall e args)
    = tc_bind2 newTvar (inferList (e : args)) f1
      where
        f1 tv ((t : ts), sm) = (f2 tv) <$> (unify t (foldr Tarr tv ts) sm) <|> err      || te = (t0 -> ... tn -> tv)
        f1 _  _              = undef    || added to remove compiler warning

        f2 tv sm             = (t_subst sm tv, sm)
        err                  = tc_errorNote $ Info ("in application of " ++ showAst e)

infer (Ecase x e alts)
    = tc_bind2 newTvar (inferList (e : alts)) f1
      where
        f1 tv ((t : ts), sm)
            = tc_foldM unify' sm ts >>= f2 tv      || unify type of all alts to common type te -> *
              where
                t2t'        = t $Tarr tv
                unify' sm t = unify t t2t' sm

        f1 _ _ = undef  || added to remove compiler warning

        f2 tv sm = tc_pure (t_subst sm tv, sm)

infer (CaseAlt (Pctor n vars) e)
    = (infer (Evar n)) >>= f1      || infer the type of the constructor
      where
        f1 (spec, sm)
            = tc_error (CtorArity n),              if #vars ~= #ts || ctor patterns must be saturated
            = lexMono tdefs infer e >>= f2 bt sm, otherwise        || infer e in new monomorphic environment of arg types
              where
                (ts, bt) = unArr spec
                tdefs    = mkTypeMap vars ts

        f2 bt sm (t, sm') = tc_pure (bt $Tarr t, subComp sm' sm)   || type of CaseAlt is base type of constructor -> te

        unArr (Tarr t1 t2) = (t1 : ts, tf) where (ts, tf) = unArr t2
        unArr t1           = ([], t1)

infer (CaseAlt (Pvar n) e)
    = newTvar >>= f1
      where
        f1 tv         = lexMono (m_singleton n tv) infer e >>= f2 tv
        f2 tv (t, sm) = tc_pure (tv $Tarr t, sm)

infer (CaseAlt (Plit e1) e2)
    = inferList [e1, e2] >>= f1
      where
        f1 ([t1, t2], sm) = tc_pure (t1 $Tarr t2, sm)
        f1 _              = undef       || added to remove compiler warning

infer (CaseAlt Pwildcard e)
    = tc_bind2 newTvar (infer e) f
      where
         f tv (t, sm) = tc_pure (tv $Tarr t, sm)        || inferred type is * -> t

|| Efatbar and Efail form a control scheme, with an Efail acting like a
|| non-local control transfer to the right branch of the enclosing Efatbar
|| It might be more correct to capture the right branch type scheme of an Efatbar (like in an Elet),
|| and then look it up for an Efail (like an Evar reference).  However, it works just to create
|| a new Tvar for its type and let inference figure it out
infer (Efail e)
    = newTvar >>= f
      where
        f tv = tc_pure (tv, id_subst)                   || let inference figure out the result type

infer (Efatbar p e1 e2)
    = tc_bind2 newTvar (inferList [e1, e2]) f1
      where
        f1 tv (ts, sm)
            = tc_foldM unify' sm ts >>= f2 tv      || unify types of e1 and e2 to a common type *
              where
                unify' sm t = unify t tv sm

        f2 tv sm = tc_pure (t_subst sm tv, sm)

|| non-recursive Let
infer (Elet False defs x e)
    = inferMap defs >>= f1         || infer defs in defnMap
      where
        f1 (ts, sm)     = lexPoly ts infer e >>= f2 sm     || infer e in new polymorphic environment of def types
        f2 sm  (t, sm') = tc_pure (t, subComp sm' sm)

|| recursive Let
infer (Elet True defs x e)
    = genTypeMap defs >>= f1                       || generate a fresh Tvar for each of the defs
      where
        f1 ts1
            = lexMono ts1 inferMap defs >>= f2     || infer def types monomorphically
              where
                f2 (ts2, sm) = (unifyMaps Infer ts1 ts2 sm >>= f3 ts2) <|> mutRecErr (m_keys defs)

                f3 ts2 sm1
                    = tc_over tcEnv (te_subst sm1) *>
                      lexPoly ts2' infer e         >>= 
                      f4 sm1
                      where
                        ts2' = m_fmap (t_subst sm1) ts2

                f4 sm1 (t, sm2)  = tc_pure (t, subComp sm2 sm1)

infer d
    = inLexEnv m_empty locInfer d,                              if isDefn d
    = error ((shows "unhandled infer case: " . showsAst d) ""), otherwise
      where
        loc        = defnLocInfo d
        locInfer d = tc_over tcLoc (mergeLocInfo loc) *> inferDef d

        inferDef (DefFn n vars e an)
            = tc_mapM (const newTvar) vars >>= f1       || create new Tvars for the arguments
              where
                f1 ts = lexMono tdefs infer e >>= f2 ts || infer the type of the body in a new mono environment
                        where
                          tdefs = mkTypeMap vars ts

                f2 ts (t, sm) = tc_pure (foldr (Tarr . t_subst sm) t ts, sm)    || final type is ts0 -> .. t

        inferDef (DefPat p e an)
            = infer e >>= f
              where
                f (t, sm) = tc_error UnboxedPat, if _eq cmptexpr t builtinTypeWord
                          = tc_pure (t, sm),     otherwise

        inferDef d = tc_error . TypeIdent . defnName $ d

|| infer a (possibly mutually-recursive group) of definitions in a type environment
inferDefs :: [defn] -> tcState [texpr]
inferDefs defs
    = tc_view tcGenv >>= f
      where
        ndefs = #defs
        ns    = map defnName defs
        def   = hd defs
        rec   = view defnAnRec def

        || infer a single defn normally, if it isn't recursive or it has an existing spec
        || otherwise, infer the definition(s) monomorphically as a recursive group
        f genv = infer def >>= wrapSingle,                      if ndefs == 1 & (~rec \/ hasSpec)
               = tc_mapM (const newTvar) defs >>= inferDefsRec, otherwise
                 where
                   hasSpec              = isJust . lookupSpec genv . defnName $ def     || polymorphic recursive fn (Milner-Mycroft Calculus)
                   wrapSingle (tdef, x) = tc_pure [tdef]

        inferDefsRec ts1
            = lexMono tm1 inferList defs >>= f2
              where
                tm1          = m_fromList cmpname $ zip2 ns ts1
                f2 (ts2, sm) = (unifyLists Infer ts1 ts2 sm >>= f3 ts2) <|> mutRecErr ns
                f3 ts2 sm'   = tc_pure $ map (t_subst sm') ts2

|| note: this cannot be performed with a tc_mapM, because it is bidirectional:
|| going forward, the phi's are computed for each expr and the environment schemes are
|| updated with them, so that the types of subsequent exprs are infered with those prior
|| constraints.  Then going backward, each prior inferred type has the subsequent phi's
|| folded into their type, to get the fixpoint for the constraints.
|| it could be performed with a right-fold version of tc_foldM, but that wouldn't be simpler than this.
inferList :: [expr] -> tcState ([texpr], substMap)
inferList [] = tc_pure ([], id_subst)
inferList (e : es)
    = infer e >>= f1
      where
        f1 (t, sm)        = tc_over tcEnv (te_subst sm) *> (inferList es >>= f2 t sm)
        f2 t sm (ts, sm') = tc_pure (t_subst sm' t : ts, subComp sm' sm)

inferMap :: defnMap -> tcState (typeMap, substMap)
inferMap dm
    = inferList defs >>= f
      where
        (ks, defs) = unzip2 $ m_toList dm
        f (ts, sm) = tc_pure (m_fromList cmpname (zip2 ks ts), sm)


|| type unification

|| unification can either be for inference, or checking that the inferred type subsumes the specified type
unifyMode ::= Infer | Check

|| extend a substMap  with a substitution, possibly reporting an error
|| if unifyMode is Check, check for an Occurs error against the free variables
|| of the original second type, which have been saved in the tcSt
extend :: unifyMode -> substMap -> int -> texpr -> tcState substMap
extend um sm tvn t
    = tc_view tcFvars >>= f
      where
        f oFvars
            = tc_pure sm,                if _eq cmptexpr t tv
            = tc_error (Occurs tvn' t'), if s_member cmpint tvn $ fvars um
            = tc_pure  extended,         otherwise
              where
                sm'         = m_singleton tvn t
                mapped      = m_fmap (t_subst sm') sm
                extended    = m_union cmpint mapped sm'
                tv          = Tvar tvn
                Tarr tv' t' = normalize $ Tarr tv t
                Tvar tvn'   = tv'                

                fvars Infer = t_ftv t
                fvars Check = oFvars

|| unify two texprs by possibly extending the substitution, or report an error
|| tcSt is passed in to provide info for error reporting, but is unmodified
unify' :: unifyMode -> texpr -> texpr -> substMap -> tcState substMap
unify' um t1 t2 sm
    = go t1 t2
      where
        go (Tvar tvn)    t2            = go t1' t2',           if m_member cmpint tvn sm        || unify 
                                       = extend um sm tvn t2', otherwise                        || extend
                                           where
                                             t1' = subst sm tvn
                                             t2' = t_subst sm t2
        go t1            (Tvar tvn)    = go (Tvar tvn) t1,       if _eq cmpunifyMode um Infer   || Infer is bidirectional; Check is one-sided
        go (Tname n1 ta) (Tname n2 tb) = unifyLists um ta tb sm, if _eq cmpname n1 n2 & #ta == #tb
        go (Ttuple ta)   (Ttuple tb)   = unifyLists um ta tb sm, if #ta == #tb
        go (Tlist ta)    (Tlist tb)    = go ta tb
        go (Tarr ta tb)  (Tarr tc td)  = unifyLists um [ta, tb] [tc, td] sm
        go (Tstrict ta)  (Tstrict tb)  = go ta tb
        go t1            t2            = tc_error $ TypeHole (normalize t2),             if isTWild t1
                                       = tc_error $ TypeHole (normalize t1),             if isTWild t2
                                       = tc_error $ Unify (normalize t1) (normalize t2), otherwise
                                         where
                                           isTWild (Tname n _) = isWildcard n
                                           isTWild _           = False

|| unify a pair of texpr lists
unifyLists :: unifyMode -> [texpr] -> [texpr] -> substMap -> tcState substMap
unifyLists um ts1 ts2 sm
    = tc_foldM unifyPair sm $ zip2 ts1 ts2
      where
        unifyPair sm (t1, t2) = unify' um t1 t2 sm

|| unify a pair of texpr maps
unifyMaps :: unifyMode -> typeMap -> typeMap -> substMap -> tcState substMap
unifyMaps um ts1 ts2 sm
    = unifyLists um (m_elems ts1) (m_elems ts2) sm

unify :: texpr -> texpr -> substMap -> tcState substMap
unify t1 t2 sm
    = unify' Infer t1 t2 sm <|>
      tc_errorNote (Unify (normalize t1) (normalize t2))    || add additional error info for the top-level unification types

subsumes :: texpr -> texpr -> substMap -> tcState substMap
subsumes t1 t2 sm
    = tc_set tcFvars (t_ftv t2) *> 
      (unify' Check t1 t2 sm <|>
      tc_errorNote (Unify (normalize t1) (normalize t2)))   || add additional error info for the top-level unification types


|| typecheck a group of mutually-recursive definitions (singleton if not recursive)
|| and validate existing specs or install new specs
|| note: some trickiness in typing used, here -- final return type is tcState (), because failures can occur
||       in inferDefs, which will return Nothing.  If they return a list of types to matchSpecs, then matchSpecs
||       runs matchOrAdd in an st monad (rather than a tc monad) to collect info from
||       all type results, rather than short-circuiting on the first failure from subsumes, then wraps the result
||       state with a tc_pure (), to match the final return type
typecheckGroup :: [name] -> tcState ()
typecheckGroup ns
    = tc_view tcGenv >>= f
      where
        f genv
            = inferDefs defs >>= matchSpecs
              where
                defs          = map (fromJust . lookupDefn genv) ns     || guaranteed to exist
                matchSpecs ts = tc_pure () $st_left st_mapM_ matchOrAdd (zip2 ns ts)

        matchOrAdd (n, t)
            = tc_view tcGenv >>= f
              where
                f genv
                  = installSpec,                                           if isNothing mspec
                  = inLexEnv m_empty locSubsumes id_subst $st_bind verify, otherwise            || verify inferred against specified type
                    where
                      mspec          = lookupSpec genv n
                      spec           = fromJust mspec
                      DefSpec _ t' _ = spec
                      normt          = normalize t
                      spec'          = DefSpec (Tname n []) normt (genvAnno genv)
                      loc            = defnLocInfo spec

                      verify Nothing = incorrDeclErr
                      verify _       = tc_pure () 

                      installSpec    = tc_over tcSpecs (m_insert cmpname n spec')
                      incorrDeclErr  = tc_over tcErrs ((Error, IncorrDecl spec spec', loc) :)
                      locSubsumes sm = tc_over tcLoc (mergeLocInfo loc) $st_right subsumes t t' sm


|| divide top-level definitions into mutually-recursive groups and typecheck each
|| group in dependency-order
typecheck :: globalEnv -> excpt globalEnv
typecheck genv
    = ex_return genv' warns, if null errs
    = ex_errs   errs,        otherwise
      where
        m              = view genvMod genv
        defs           = m_filter cmpname isDefFnPat (view modDefns m)
        specs          = m_elems (view modSpecs m)
        st             = foldr  addNoDefWarn (initTcSt genv) (filter noDef specs)
        deps           = makeDefnDependencyList defs
        sccs           = findSCCs cmpname . makeDependencyGraph cmpname $ deps
        st'            = snd $ st_mapM_ typecheckGroup sccs st       || note: st_mapM_ to run typecheck on all groups
        genv'          = view tcGenv st'
        (errs, warns)  = partition isError (view tcErrs st')

        noDef d        = ~m_member cmpname (defnName d) defs
        addNoDefWarn d = addTcWarn (SpecNoDef (defnName d)) (locOnlyInfo (view defnAnLoc d))
