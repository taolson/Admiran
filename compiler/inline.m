|| inline.m -- inlining saturated calls to known non-recursive functions and simplification transformations
||
|| todo: currently the analysis info (free vars, uses, complexity) is recomputed after each inlining pass for the function group being
||       inlined.  This might be able to be changed to updating those vars during the inline pass, rather than re-performing the
||       analysis each time.  Performace doesn't seem to bad right now, though.
||
|| re-calculation of fuel when eliminating dead code seems wrong -- fuel can actually increase above the original limit
||
|| look into idea of join points -- let-bound tail calls of fully-saturated functions, and tag them as such (in analyzer?)
|| when generating code for a join point, let does no allocation, and the call is just an "adjust stack pointer and jump"
|| Paper: "Compiling Without Continuations"


%export inline streamSubst

%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>                 (>>=)/st_bind
%import "analyze"
%import "ast"
%import "config"
%import "dependency"
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"
%import "rename"

|| functions which don't return (call error and terminate)
noReturnNames = map (Qualified "stdlib") ["exit", "error", "blackHole", "undef", "caseFail", "matchFail", "cmpFn"]

|| functions we shouldn't inline
isIOfn :: name -> bool
isIOfn n = getModName n ==$ "io"

|| functions we shouldn't inline
noInlineNames :: [name]
noInlineNames = map (Qualified "stdlib") ["trace", "getTag", "cmpTags"]                ++
                map (Qualified "stream") ["toStream", "fromStream", "readByteStreamS"] ++
                noReturnNames

|| builtin functions that can be inlined by computing at compile-time
inlineBuiltinNames = ["cmp#", "+#", "-#", "*#", "div#", "mod#"]


|| a localEnv consists of a stack of frames, which are either a function boundary marker
|| or a defnMap.  The function boundary marker is used to indicate whether we have crossed
|| a function definition boundary when performing a lookup, to later determine viability
|| of inlining the looked-up name (don't inline complex exprs if they get moved into a fnDefn that
|| could be executed multiple times)
frame    ::= FnBound | Env defnMap
localEnv == [frame]

|| state passed through inline
|| dynamic state:
|| genv:       global environment
|| subst:      name substitution map (for substituting stream versions of list functions)
|| fuel:       complexity remaining allotted to subsequent inlining
|| simplified: bool indicating that simplification was performed (controls iteration on inlining and simplify)
|| errs:       exceptions (notes)
||
|| lexically-scoped state:
|| lenv: local lexical environment
|| loc:  locInfo
inDynSt  == (globalEnv, nameMap, num, bool, [exception])
inLexSt  == (locInfo, localEnv)
inSt     == (inDynSt, inLexSt)

|| lenses for inSt fields
inDyn        = lensFst
inLex        = lensSnd
inGenv       = composeLens inDyn lensTup5_0
inSubst      = composeLens inDyn lensTup5_1
inFuel       = composeLens inDyn lensTup5_2
inSimplified = composeLens inDyn lensTup5_3
inErrs       = composeLens inDyn lensTup5_4
inLoc        = composeLens inLex lensFst
inLenv       = composeLens inLex lensSnd
inLocation   = composeLens inLoc locInfoLocation
inDefnPath   = composeLens inLoc locInfoDefnPath

initInSt :: globalEnv -> nameMap -> inSt
initInSt genv subst = ((genv, subst, inlineMaxComplexity, False, []), (emptyLocInfo, []))

|| add a note to the inline state, limiting the length of the note string
addNote :: string -> inSt -> inSt
addNote s st
    = over inErrs ((Note, info, emptyLocInfo) :) st, if inlineVerbose
    = st,                                            otherwise
      where
        info = Info $ limitInfo noteLengthLimit s

|| attach additional info to the most recent exception or create a new Note
addInfo :: string -> inSt -> inSt
addInfo s st
    = over inErrs attach st, if inlineVerbose
    = st,                    otherwise
      where
        info = Info $ limitInfo noteLengthLimit s

        attach []                          = [(Note, info, emptyLocInfo)]
        attach ((et, ExMult eis, el) : es) = (et, ExMult (eis ++ [info]), el) : es
        attach ((et, ei, el) : es)         = (et, ExMult [ei, info], el) : es

|| lookup the defn bound to a name in the local or global environments, along with
|| an indication of whether the lookup crossed a function definition boundary
lookupEnv :: name -> inSt -> (maybe defn, bool)
lookupEnv n st
    = lookFrame (view inLenv st) False, if m_null (view inSubst st)
    = lookStream,                       otherwise
      where

        lookFrame []             cf = (lookupDefn (view inGenv st) n, cf)       || name not found in Lenv, check globalEnv
        lookFrame (FnBound : fs) cf = lookFrame fs True                         || crossed a DefFn boundary
        lookFrame (Env e : fs)   cf
            = (md, cf),        if isJust md
            = lookFrame fs cf, otherwise
              where
                md = m_lookup cmpname n e

        || only allow functions from the stream module to be inlined
        lookStream
            = (md,      False), if _eq cmpstring (getModName n) streamModuleName
            = (Nothing, False), otherwise
              where
                md = lookupDefn (view inGenv st) n

|| rewrite an expr, directly replacing var references with the corresponding value of the binding
replace :: expr -> [(name, expr)] -> expr
replace e bdgs
    = e,  if null bdgs
    = e', otherwise
      where
        env = m_fromList cmpname bdgs
        e'  = st_evalState (astRewriteTD replaceAst e) ()

        replaceAst (Evar n) = st_pure $ m_findWithDefault cmpname (Evar n) n env
        replaceAst a        = st_pure a

|| rewrite only case scrutinees in an expr, replacing var references with the corresponding value of the binding
replaceScrutinee :: expr -> [(name, expr)] -> expr
replaceScrutinee e bdgs
    = e,  if null bdgs
    = e', otherwise
      where
        e' = st_evalState (astRewriteTD replaceAst e) ()

        replaceAst (Ecase b e alts) = st_pure $ Ecase b (replace e bdgs) alts
        replaceAst a                = st_pure a

|| wrap an expr in a sequence of individual let bindings created from a list of names and exprs
wrapInLet :: location -> expr -> [(name, expr)] -> expr
wrapInLet loc e bdgs
    = foldr wrap e bdgs
      where
        an = set anUses 1 $ locAnno loc

        wrap (n, e') e
            = Elet False (m_singleton n d) [] e
              where
                d = DefPat (Pvar n) e' an

|| create a binding of args to vars, directly rewriting simpleExpr args,
|| and wrapping the rest in a let binding
createBinding :: location -> expr -> [pat] -> [expr] -> expr
createBinding loc e vars args
    = wrapInLet loc (replace e smp) cpx
      where
        (smp, cpx) = partition (isSimpleExpr . snd) . map (mapFst unPvar) . filter (isPvar . fst) . zip2 vars $ args

|| perform beta-reduction on a defn with arguments, renaming all the Pvars in the defn first
|| to prevent name capture
betaReduce :: defn -> [expr] -> state inSt expr
betaReduce d args st
    = (e2, st')
      where
        loc            = fromMaybe emptyLocation $ view inLocation st
        (d', genv')    = renameExpr "`" d $ view inGenv st      || rename inlined defn, prefixing included defns to hide them in reports
        st'            = set inGenv genv' st
        vars           = fnVars d'
        e              = fnExpr d'
        (args1, args2) = splitAt (#vars) args
        e1             = createBinding loc e vars args1
        e2             = e1,               if null args2
                       = (Ecall e1 args2), otherwise

|| see if name is something we can replace with its definition
canInline :: name -> bool
canInline n = ~(isBuiltin n \/ isConstructor n \/ isIOfn n \/ member cmpname noInlineNames n)

canInlineInternal :: name -> bool
canInlineInternal n = isInternal n & canInline n

|| see if expr isSimpleExpr or a simple constructor
isSimple :: expr -> bool
isSimple (Ecall (Evar c) args) = isConstructor c & all isSimpleExpr args
isSimple e                     = isSimpleExpr e

|| see if expr can return a value, or if it terminates
|| (used when rewriting nested case exprs, to prevent expansion of a non-returning expr)
noReturn :: expr -> inSt -> bool
noReturn (Evar n) st
    = member cmpname noReturnNames n \/ fromMaybef False checkThunk (fst (lookupEnv n st))
      where
        checkThunk (DefPat _ e _) = noReturn e st
        checkThunk m              = False

noReturn (Ecall e args) st = noReturn e st
noReturn e              st = False


inlineAst :: astRewriteTraverser inSt

|| early out of the rest of inlining, if out of fuel
inlineAst k e st = (e, st), if view inFuel st <= 0

|| do inlining on the defs of an Elet, then add that as a new lexical environment for inlining the expr of the Elet
inlineAst k (Elet rec defs x e) st
    = (Elet rec defs' x e', st2)
      where
        (defs', st1) = astRewriteMap (astRewriteLex inlineAst) defs st                  || do inlining on all the defns, separately
        (e', st2)    = astRewriteLex inlineAst e (over inLenv (Env defs' :) st1)        || then do inlining on the let expr, with the new defns

|| inline builtinSel on constant ctor call
inlineAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [Ecall (Evar c) args]) st
    = k (args ! idx) st', if useSel & isConstructor c
      where
        note = "inlined builtinSel on constant ctor"
        st' = addInfo note . set inSimplified True $ st

|| inline builtinSel on var which is bound to a ctor call
|| note: this could be a sel for a data item which should be returned, or it could be a sel on the first
|| argument of a call, with the selector selecting a fn that gets passed the remaining arguments of the call
|| prime example is Lens (getf/overf) functions
inlineAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) (Evar n : args)) st
    = k selArg              st', if useSel & inlinable & null args
    = k (Ecall selArg args) st', if useSel & inlinable
      where
        inlinable     = isJust md & isDefPat d & isJust margs'
        note          = "inlined builtinSel on " ++ showName n
        st'           = addInfo note . set inSimplified True $ st
        md            = fst $ lookupEnv n st
        d             = fromJust md
        DefPat _ e _  = d
        margs'        = ctorCallArgs e
        args'         = fromJust margs'
        selArg        = args' ! idx

        ctorCallArgs (Ecall (Evar c) args) = Just args, if isConstructor c
        ctorCallArgs e                     = Nothing

|| compute some builtin functions on literal unboxed int values -- useful for, e.g. determining
|| rangeByFromTo loop direction at compile time, or removing dead code during specialization of
|| case expressions
inlineAst k (Ecall (Evar (Builtin fn)) [(EprimInt a), (EprimInt b)]) st
    = (EprimInt r, st'), if member cmpstring inlineBuiltinNames fn
      where
        st' = addInfo ("inlined builtin " ++ fn) . set inSimplified True $ st
        r   = doBuiltin fn a b

        doBuiltin "+#"   a b = a + b
        doBuiltin "-#"   a b = a - b
        doBuiltin "*#"   a b = a * b
        doBuiltin "div#" a b = a $div b
        doBuiltin "mod#" a b = a $mod b
        doBuiltin "cmp#" a b = 0, if a == b
                             = 1, if a < b
                             = 2, otherwise
        doBuiltin _      _ _ = undef    || added to remove compiler warning

|| inline builtin cmp# on literal unboxed char values
inlineAst k (Ecall (Evar (Builtin "cmp#")) [(EprimChar a), (EprimChar b)]) st
    = (EprimInt r, st')
      where
        st' = addInfo "inlined builtin cmp" . set inSimplified True $ st
        r   = 0, if a ==. b
            = 1, if a <. b
            = 2, otherwise

|| collapse toStream/fromStream pairs
|| note: the fromStream may be the final expr in a chain of Elets, so we need to walk
|| the chain to check the final expr
inlineAst k (Ecall (Evar ts) [e]) st
    = k e' st', if _eq cmpname ts toStreamName & isJust me'
      where
        me'            = walkLets e
        e'             = fromJust me'
        toStreamName   = Qualified streamModuleName "toStream"
        fromStreamName = Qualified streamModuleName "fromStream"
        note           = "collapsed toStream/fromStream"
        st'            = addInfo note . set inSimplified True $ st

        walkLets (Elet rec defs x e)   = Elet rec defs x $mb_fmap walkLets e
        walkLets (Ecall (Evar fs) [e]) = Just e, if _eq cmpname fs fromStreamName
        walkLets e                     = Nothing

|| substitute stream versions of list functions
inlineAst k (Ecall (Evar n) args) st
    = k (Ecall (Evar n') args) st', if isJust mn'
      where
        mn'  = m_lookup cmpname n (view inSubst st)
        n'   = fromJust mn'
        note = "substituted stream version of " ++ showName n
        st'  = addInfo note . set inSimplified True $ st

|| inline the function body of a call
|| if inlineable, return without continuing through the new inlined body, to allow more inlining
|| of other functions at the same "level", first.  If enough fuel remains, another pass will continue
|| inlining through the new inlined body
|| NOTE: this will aggressively inline thunks, even if they are expensive, causing duplication of work (and
|| memoization to fail).  However, removing the possibility entirely causes a big performance drop.  Need to
|| figure out the right balance, here.
inlineAst k (Ecall (Evar n) args) st
    = (e, st2), if inlinable
      where
        inlinable = canInline n & isJust md & ~view defnAnRec d & fnArity d <= #args & cpx <= inlineMaxFnComplexity
        (md, _)   = lookupEnv n st
        d         = fromJust md
        cpx       = view defnAnCpx d
        (e, st1)  = betaReduce d args st
        note      = "inlined function " ++ showName n
        st2       = addInfo note . over inFuel (subtract cpx) . set inSimplified True $ st1

|| do inlining of an Evar in a case scrutinee
|| if scrutinee is not a known let-bound var, bind its name to each alt pattern type in the alts (to specialize its value in each alt)
|| Note that in some cases, the binding of a var to an alt pattern will cause extra work of building a new ctor, instead of using the
|| exisiting var.  However, it overall is a performance improvement, due to the subsequent optimizations the specialization provides.
|| Note: this used to allow inlining of singleCpx values, since theoretically they are only used once here (in the case scrutinee), but
|| a case was found where an expensive fn call was inlined twice during expansion of stdlib.(!).  Need to figure out why uses count
|| was one, in that case, but for now we only inline simple exprs.
inlineAst k (Ecase b (Evar n) alts) st
    = k (Ecase b e'       alts)  st2, if inlinable      || inline the case scrutinee
    = k (Ecase b (Evar n) alts') st3, if isInternal n   || bind the name to the individual caseAlt patterns
    = k (Ecase b (Evar n) alts)  st,  otherwise         || don't do anything if the name is a top-level pat
      where
        inlinable    = canInlineInternal n & isJust md & ~view defnAnRec d & isDefPat d & isSimple e & cpx <= inlineMaxFnComplexity
        (md, _)      = lookupEnv n st
        d            = fromJust md
        cpx          = view defnAnCpx d
        e            = fnExpr d
        (e', st1)    = betaReduce d [] st
        note         = "inlined case scrutinee " ++ showName n
        st2          = addInfo note . over inFuel (subtract cpx) . set inSimplified True $ st1
        (alts', st3) = st_mapM bindInAlt alts st

        bindInAlt alt st
            = (alt, st),                                                      if isNothing me
            = (replaceScrutinee alt [(n, e)], over inFuel (subtract cpx) st), otherwise
              where
                me       = mkExpr alt
                (e, cpx) = fromJust me

                mkExpr (CaseAlt (Plit l) e)       = Just (l, 1)
                mkExpr (CaseAlt (Pctor c vars) e) = Just (Ecall (Evar c) (map (Evar . unPvar) vars), 2 + #vars), if all isPvar vars
                mkExpr alt                        = Nothing

|| do inlining of an expr as a caseAlt value
|| Note: since we are strictly evaluating the expr, we can inline complex exprs that have a single use, since we know
|| it won't be passed to some other function which might replicate its use
inlineAst k (CaseAlt p (Evar n)) st
    = k (CaseAlt p e') st2, if inlinable
      where
        inlinable     = canInlineInternal n & isJust md & ~view defnAnRec d & isDefPat d & (isSimple e \/ singleCpx) & cpx <= inlineMaxFnComplexity
        (md, cf)      = lookupEnv n st
        d             = fromJust md
        uses          = view defnAnUses d
        cpx           = view defnAnCpx d
        singleCpx     = uses == 1 & ~cf
        e             = fnExpr d
        (e', st1)     = betaReduce d [] st
        note          = "inlined caseAlt expr " ++ showName n
        st2           = addInfo note . over inFuel (subtract cpx) . set inSimplified True $ st1

|| do inlining of a simple expr
|| Note: this used to also include cases of single-use complex exprs, but it turns out that the use count from analyze isn't
|| enough to determine dynamic uses at runtime: a case happened where it statically looked like a single-use case, but in reality
|| was passed to a function which replicated it many times, resulting in an O(n^2) performance problem for an expensive thunk.
|| now we only inline simpleExprs
inlineAst k (Evar n) st
    = (e', st2),    if inlinable
    = (Evar n, st), otherwise
      where
        inlinable     = canInline n & isJust md & ~view defnAnRec d & isDefPat d & (isSimple e \/ singleCpx) & cpx <= inlineMaxFnComplexity
|| DEBUG        inlinable     = canInlineInternal n & isJust md & ~view defnAnRec d & isDefPat d & (isSimple e \/ singleCpx) & cpx <= inlineMaxFnComplexity
        (md, cf)      = lookupEnv n st
        d             = fromJust md
        uses          = view defnAnUses d
        cpx           = view defnAnCpx d
        singleCpx     = uses == 1 & ~cf
        DefPat _ e _  = d
        (e', st1)     = betaReduce d [] st
        note          = "inlined expr " ++ showName n
        st2           = addInfo note . over inFuel (subtract cpx) . set inSimplified True $ st1

|| add locInfo for defn, and a FnBound frame to the local env when entering a DefFn
inlineAst k a st
    = k a st2, if isDefn a
    = k a st,  otherwise
      where
        loc = defnLocInfo a
        st1 = over inLoc (mergeLocInfo loc) st
        st2 = over inLenv (FnBound :) st1, if isDefFn a
            = st1,                         otherwise


|| simplifyAst -- perform a series of simplification transformations on Ecase and Elet exprs that can
|| expose more opportunities for inlining (from the paper "Let-floating: moving bindings to give faster programs"
|| by Simon Peyton Jones
simplifyAst :: astRewriteTraverser inSt

|| don't do any simplification during stream substitution, or if out of fuel
simplifyAst k a st
    = (a, st), if ~m_null (view inSubst st) \/ view inFuel st <= 0

|| float a let into caseAlts, if unused in the case scrutinee
|| this will cause the lets to be specialized in each alt, allowing unused lets to be removed
|| (fewer runtime bindings created)
simplifyAst k (Elet rec defs x (Ecase b e alts)) st
    = k (Ecase b e alts') st', if ~used
      where
        cpx   = sum . map (view defnAnCpx) . m_elems $ defs
        used  = astAccumTD tallyUses e False
        alts' = map insLet alts
        note  = "float let into case alts"
        st'   = addInfo note . over inFuel (subtract (cpx * (#alts - 1))) . set inSimplified True $ st

        || handle the degenerate case where the CaseAlt is an Evar ref for a defn in the let, by replacing it directly
        || this gets rid of extraneous thunk creation / updates which couldn't be optimized away by the inlining of
        || a caseAlt value (introduced Let bindings cause that pattern to fail)
        insLet (CaseAlt p (Evar n))
            = CaseAlt p e', if isJust md & ~view defnAnRec d & isDefPat d
              where
                md = m_lookup cmpname n defs
                d  = fromJust md
                DefPat _ e' _ = d

        insLet (CaseAlt p e) = CaseAlt p (Elet rec defs x e)
        insLet _             = undef    || added to remove compiler warning

        tallyUses (Evar n) used = used \/ m_member cmpname n defs
        tallyUses a        used = used

|| dead code elimination for unused definitions (that have been inlined), and rewrite of the Elet to its
|| expr if all defns are removed.  Note: this pattern comes after the previous simplification to an Elet, because
|| this Elet pattern is more general, and would make the previous unreachable
simplifyAst k (Elet b defs x e) st
    = (k (Elet b defs x e) >>= f) (over inLenv (Env defs :) st)
      where
        f (Elet rec defs pats e) st
            = (e, st'),                     if null ds
            = (Elet rec defs' pats e, st'), otherwise
              where
                (ds, dead) = partition (isUsed . snd) (m_toList defs)
                fuel'      = sum (map (view defnAnCpx . snd) dead)
                defs'      = m_fromList cmpname ds
                notes      = ("increased fuel by " ++ showint fuel') : map (("eliminated " ++) .  showName . defnName . snd) dead
                st'        = st,                                               if null dead
                           = over inFuel (fuel' +) . foldr addInfo st $ notes, otherwise
                isUsed d   = view defnAnUses d - selfRefUse d > 0

                || does a recursive defn still use itself, or has that been eliminated?
                || note: this is conservative: a recursive function could reference itself multiple times, but we will
                || return only 0 or 1 here, since we only know it is referenced via the defnAnFree set, rather than its count.
                selfRefUse d
                    = 1, if view defnAnRec d & s_member cmpname (defnName d) (view defnAnFree d)
                    = 0, otherwise

         f _ _ = undef    || added to remove compiler warning

|| simplify case exprs that don't return
simplifyAst k (Ecase b e alts) st
    = k e st', if noReturn e st
      where
        st' = addInfo "simplified no-return scrutinee" . set inSimplified True $ st

|| simplify builtin functions that have identity operands
|| note that this pattern match uses the more complex (Ecase (Ecall ..) [CaseAlt ..]) pattern, rather than
|| the simpler inner Ecall, to allow us to collapse the entire expr
simplifyAst k (Ecase sel (Ecall (Evar (Builtin fn)) [ea, eb]) [CaseAlt (Pvar v) e]) st
    = checkIdentAdd ea eb, if fn ==$ "+#"
    = checkIdentSub ea eb, if fn ==$ "-#"
    = checkIdentMul ea eb, if fn ==$ "*#"
      where
        st'     = addInfo ("inlined identity on builtin " ++ fn) . set inSimplified True $ st
        base    = k (Ecase sel (Ecall (Evar (Builtin fn)) [ea, eb]) [CaseAlt (Pvar v) e]) st
        repl e' = k (replace e [(v, e')]) st'

        checkIdentAdd (EprimInt 0) eb = repl eb
        checkIdentAdd ea (EprimInt 0) = repl ea
        checkIdentAdd _  _            = base

        checkIdentSub ea (EprimInt 0) = repl ea
        checkIdentSub _  _            = base

        checkIdentMul (EprimInt 1) eb = repl eb
        checkIdentMul ea (EprimInt 1) = repl ea
        checkIdentMul _  _            = base

|| swap order of an Ecase where the scrutinee is also an Ecase
|| note: this duplicates alts1 #alts2 times. Since we are duplicating alts1, and there
|| is a chance that we might float a let defined in one outside of it, we could run into
|| name collision problems.  To solve this, we rename each duplicate.
simplifyAst k (Ecase b1 (Ecase b2 tst alts2) alts1) st
    = k (Ecase b2 tst alts2') st2, if #alts1 * #alts2 <= inlineMaxCaseDup
      where
        cpx = analyzeExprCpx (Ecase b1 builtinFail alts1) (view inGenv st)    || template for added cost of pushing outer alts
        (alts2', st1) = st_mapM pushAlt alts2 st
        note          = "swapped order of case (case)"
        st2           = addInfo note . over inFuel (subtract (cpx * (#alts2 - 1))) . set inSimplified True $ st1

        || push a duplicate of alts1 into an alt2
        pushAlt (CaseAlt p e) st
            = (CaseAlt p e, st),                   if noReturn e st     || e doesn't return, so don't replace it
            = (CaseAlt p e', set inGenv genv' st), otherwise
              where
                (e', genv') = renameExpr "" (Ecase b1 e alts1) (view inGenv st)

        pushAlt _ _ = undef       || added to remove compiler warning

|| simplify an Ecase with a constant constructor scrutinee by replacing it with its corresponding caseAlt
|| this is the main simplification optimization which the others are trying to expose
simplifyAst k (Ecase b e alts) st
    = k e' st', if isJust ma
      where
        genv = view inGenv st
        loc  = fromMaybe emptyLocation (view inLocation st)
        ma   = (extrCtorArgs e $mb_bind matchCtor) $mb_alt (extrLit e $mb_bind matchLit)
        e'   = fromJust ma
        cpx1 = analyzeExprCpx (Ecase b e alts) genv
        cpx2 = analyzeExprCpx e' genv
        note = "simplified constant ctor scrutinee " ++ showAst e
        st'  = addInfo note . over inFuel (+ (cpx1 - cpx2)) . set inSimplified True $ st

        extrCtorArgs (Evar c)              = Just (c, []),   if isConstructor c
        extrCtorArgs (Ecall (Evar c) args) = Just (c, args), if isConstructor c
        extrCtorArgs e                     = Nothing

        matchCtor (c, args)
            = foldr matchAlt Nothing alts
              where
                matchAlt (CaseAlt (Pctor c' vars) e) k
                    = Just (createBinding loc e vars args), if _eq cmpname c' c

                matchAlt (CaseAlt Pwildcard e) k = Just e

                matchAlt (CaseAlt (Pvar n) e) k
                    = Just (createBinding loc e [Pvar n] [ce])
                      where
                        ce = Evar c,                if null args
                           = (Ecall (Evar c) args), otherwise

                matchAlt a k = k

        extrLit (EprimInt n) = Just n
        extrLit e            = Nothing

        matchLit n
            = foldr matchAlt Nothing alts
              where
                matchAlt (CaseAlt (Plit (EprimInt n')) e) k = Just e, if n' == n
                matchAlt (CaseAlt Pwildcard e)            k = Just e
                matchAlt (CaseAlt (Pvar v) e)             k = Just (createBinding loc e [Pvar v] [EprimInt n])
                matchAlt a                                k = k

|| float a let outside of a case scrutinee
|| since the case evaluation is strict, the Elet bindings will always be evaluated, and this
|| exposes opportunities to match case evaluation of a constant or simple expr
simplifyAst k (Ecase b1 (Elet b2 defs x e) alts) st
    = k (Elet b2 defs x (Ecase b1 e alts)) st'
      where
        note = "float let out of case scrutinee"
        st'  = addInfo note . set inSimplified True $ st

|| this is needed when a partially-applied Ecall gets inlined
simplifyAst k (Ecall (Ecall f args2) args1) st
    = k (Ecall f (args2 ++ args1)) st'
      where
        st' = addInfo "merged partially-applied call" . set inSimplified True $ st

|| swap order of a builtinSel where the scrutinee is an Ecase
simplifyAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [Ecase b tst alts]) st
    = k (Ecase b tst (map pushSel alts)) st', if useSel
      where
        cpx  = analyzeExprCpx (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [builtinFail]) (view inGenv st) || template for added cost of pushing sel
        note = "swapped order of sel (case)"
        st'  = addInfo note . over inFuel (subtract (cpx * (#alts - 1))) . set inSimplified True $ st

        pushSel (CaseAlt p e) = (CaseAlt p (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [e]))
        pushSel _             = undef   || added to remove compiler warning

|| float a case outside of an Ecall fn
|| exposes opportunities to inline the called fn, as it becomes specialized in each alt
simplifyAst k (Ecall (Ecase b e alts) args) st
    = k (Ecase b e alts') st'
      where
        alts' = map insCall alts
        note  = "float case out of call fn"
        st'   = addInfo note . set inSimplified True $ st

        insCall (CaseAlt p e) = CaseAlt p (Ecall e args)
        insCall _             = undef   || added to remove compiler warning

|| float let outside of an Ecall fn
|| exposes opportunities to inline the called fn
simplifyAst k (Ecall (Elet b defs x f) args) st
    = k (Elet b defs x (Ecall f args)) st'
      where
        note = "float let out of call fn"
        st'  = addInfo note . set inSimplified True $ st

|| remove FATBARs that no longer have the possibility of failing
simplifyAst k (Efatbar (Pvar fn) e1 e2) st
    = k e1 st', if fc == 0
      where
        fc  = astAccumTD countFails e1 0
        st' = addInfo "removed FATBAR" . set inSimplified True $ st

        countFails (Efail (Evar fn')) n = n + 1, if _eq cmpname fn' fn
        countFails e n                  = n

simplifyAst k a st = k a st


|| do inlining on a group of possibly mutually-recursive definitions by iterating passes of inlineDefs until either
|| no more inlining/simplification is performed or we hit a limit of inline iterations or fuel.
|| if the inlining finishes by running out of iterations or fuel, it likely would be too large to be beneficial, so it is discarded
|| and the code reverts to the un-inlined version
inlineGroup :: defnMap -> [name] -> state inSt defnMap
inlineGroup defs ns st
    = (m_union cmpname defs ndefs2, st3), if ni < inlineMaxIterations & view inFuel st3 > 0  || add inlined version if inlining was succesful
    = (m_union cmpname defs ndefs1, st3), otherwise                                          || revert to original if could only inline partial
      where
        st1               = addNote ("--- " ++ intercalate " " (map showName ns) ++ " ---") st
        ndefs1            = foldl addDef m_empty ns
        st2               = set inLenv [Env defs] . set inFuel (inlineMaxComplexity * #ns) . set inSimplified True $ st1
        (ndefs2, st3, ni) = hd . dropWhile canInlineMore . iterate inlineDefs $ (ndefs1, st2, 0)

        addDef m n
            = m_insert cmpname n (fromJust md) m, if isJust md
            = m,                                  otherwise
              where
                md = fst (lookupEnv n st1) $mb_alt lookupDefn (view inGenv st) n

        canInlineMore (d, st, i)
            = i < inlineMaxIterations & view inSimplified st & view inFuel st > 0

        || note: instead of trying to track and adjust defnAnno values (cpx, free, uses) during
        || inlining and simplification, we just re-run analyze after inline and simplify, at least for now
        inlineDefs (ndefs, st, i)
            = (ndefs4, stLim, i),   if limited
            = (ndefs4, st3, i + 1), otherwise
              where
                (ndefs1, st1) = astRewriteMap (astRewriteLex inlineAst) ndefs (set inSimplified False st)
                ndefs2        = analyzeDefns ndefs1 (view inGenv st1)
                (ndefs3, st2) = astRewriteMap (astRewriteLex simplifyAst) ndefs2 st1
                ndefs4        = analyzeDefns ndefs3 (view inGenv st2)
                st3           = addInfo ("iteration: " ++ showint i ++ " fuel: " ++ showint (view inFuel st2) ++ "\n") st2
                limited       = i >= inlineMaxIterations \/ view inFuel st2 <= 0
                stLim         = set inSimplified False . addInfo "*** inlineMaxIterations limit reached ***" $ st2,   if i >= inlineMaxIterations
                              = set inSimplified False . addInfo "*** inlineMaxComplexity limit reached ***"   $ st2, otherwise

inline :: globalEnv -> excpt globalEnv
inline genv
    = ex_return genv  [],    if ~useInlining
    = ex_return genv' warns, if null errs
    = ex_errs errs,          otherwise
      where
        m              = view genvMod genv
        sccs           = findSCCs cmpname . makeDependencyGraph cmpname . makeDefnDependencyList . view modDefns $ m
        (defs', st)    = st_foldM inlineGroup m_empty sccs (initInSt genv m_empty)
        genv'          = over genvMod (set modDefns defs') (view inGenv st)
        (errs, warns)  = partition isError (view inErrs st)


|| make the stream function substitution map, by finding all of the stream functions that end with a single quote ('),
|| and finding the corresponding names for the same functions without the quote
mkStreamSubstMap :: globalEnv -> nameMap
mkStreamSubstMap genv
    = m_empty, if isNothing msubns
    = subMap,  otherwise
      where
        msubns = (filter (isSubChar . last . getName) . m_keys . view modDefns) $mb_fmap lookupModule genv "stream"     || get the compatible substitution fns
        subns  = fromJust msubns
        mns    = map (lookupName genv . Unqualified . init . getName) subns                                             || get the names they will replace
        subMap = m_fromList cmpname . map (mapFst fromJust) . filter (isJust . fst) . zip2 mns $ subns                  || make the substitution name map

        isSubChar c = c ==. '_' \/ c ==. '@'

|| stream substitution and fusion
|| replace selected list functions with compatible stream versions, then use the inlining
|| mechanism (restricted to only stream functions) to further inline those operations and
|| fuse paired toStream/fromStream functions
streamSubst :: globalEnv -> excpt globalEnv
streamSubst genv
    = ex_return genv  [],    if ~useStreamFusion
    = ex_return genv' warns, if null errs
    = ex_errs errs,          otherwise
      where
        m             = view genvMod genv
        subst         = mkStreamSubstMap genv
        sccs          = findSCCs cmpname . makeDependencyGraph cmpname . makeDefnDependencyList . view modDefns $ m
        (defs', st)   = st_foldM inlineGroup m_empty sccs (initInSt genv subst)
        genv'         = over genvMod (set modDefns defs') (view inGenv st)
        (errs, warns) = partition isError (view inErrs st)
