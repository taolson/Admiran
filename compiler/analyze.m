|| analyze.m -- perform free variable, usage, and complexity analysis on an AST


%export analyze analyzeWarn analyzeDefns analyzeExprCpx analyzeExprFree

%import <bag>
%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>                 (>>=)/st_bind (<<)/st_left
%import "ast"
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"


|| state passed through analyze:
|| dynamic state:
|| errs:      exceptions collected during traversal
|| uses:      in-scope used variable names and counts, which will used to determine free variables and use counts of defns
|| cpx:       ast complexity
||
|| lexically-scoped state:
|| loc:       location info from enclosing definitions
azDynSt   == ([exception], b_bag name, int)
azLexSt   == locInfo
azSt      == (azDynSt, azLexSt)

|| lenses for azSt fields
azDyn       = lensFst
azLex       = lensSnd
azErrs      = composeLens azDyn  lensTup3_0
azUses      = composeLens azDyn  lensTup3_1
azCpx       = composeLens azDyn  lensTup3_2
azLoc       = azLex
azDefnPath  = composeLens azLoc  locInfoDefnPath

initAzSt :: azSt
initAzSt = (([], b_empty, 0), emptyLocInfo)

|| add a warning to the analyze state
addAzWarn :: exceptionInfo -> azSt -> azSt
addAzWarn ei st = over azErrs ((Warn, ei, view azLoc st) :) st

|| check name for match with stdCaseFail to report non-exhaustive patterns in function definition
checkFail :: name -> azSt -> azSt
checkFail n st
    = addAzWarn NonExhaustive st, if _eq cmpname n stdCaseFailName
    = st,                         otherwise

|| clear the cpx and uses fields
clrCpxUses :: azSt -> azSt
clrCpxUses = set azCpx 0 . set azUses b_empty

|| add a use of a name to the use count
addUse :: name -> azSt -> azSt
addUse n st = over azUses (b_insert cmpname n) st

|| add a use of a Tname to the use count
addTUse :: name -> azSt -> azSt
addTUse n st
    = over azUses (b_insert cmpname n) st, if ~isConstructor n
    = st,                                  otherwise

|| delete a list of names from the uses (to remove variable names after they go out of scope)
|| note that this can be done by deleting keys, since renaming has guaranteed that all names are
|| now unique, so we can keep them in a single bag, rather than lexically scope them
delUses :: [name] -> azSt -> azSt
delUses ns
    = over azUses delNames
      where
        delNames uses = foldl delName uses ns
        delName b n   = b_deleteKey cmpname n b

|| add to the complexity count
addCpx :: int -> azSt -> azSt
addCpx n = over azCpx (+ n)

incCpx :: azSt -> azSt
incCpx = over azCpx (+ 1)


|| setUseCount is an astRewriteTraverser that is used to update the first-level defn annos
|| with their use counts in the current azUses st.  It also reports a warning if the
|| defn is unused
setUseCount :: astRewriteTraverser azSt
setUseCount k a st
    = (a', st'), if isDefn a    || note: only process the first-level defns and return without traversing deeper
    = k a st,    otherwise
      where
        n        = defnName a
        count    = b_findWithDefault cmpname 0 n $ view azUses st
        a'       = set defnAnUses count a
        topLevel = (null . view azDefnPath) st
        st'      = addAzWarn (Unused n) st, if count == 0 & ~topLevel
                 = st,                      otherwise

|| analyzeAst is an astRewriteTraverser that is passed an explicit continuation to use to
|| traverse its subexpressions; this can be replaced with a custom traverser, or discarded to prevent further traversal.
|| during traversal, it collects information on variable name usage, and ast complexity
|| to create free var, use count, and complexity info for defns
analyzeAst :: astRewriteTraverser azSt

|| Tally the free vars, use count, and complexity in the defn anno structure
analyzeAst k (Elet x1 defs x2 e) st
    = (Elet x1 defs2 x2 e', st4)
      where
        (defs1, st1)  = astRewriteMap (astRewriteLex analyzeAst) defs st                || analyze the let defns
        (e', st2)     = astRewriteLex analyzeAst e st1                                  || analyze the let expr
        (defs2, st3)  = astRewriteMap (astRewriteLex setUseCount) defs1 st2             || update the let defns with their use counts from other defns
                                                                                        || and the expr
        st4           = delUses (m_keys defs2) . incCpx $ st3


|| add name references to uses and remove references to defining patterns
analyzeAst k (Efatbar p e1 e2) st = k (Efatbar p e1 e2) << st_modify (delUses (patNames p) . incCpx)              $ st
analyzeAst k (Evar n)          st = k (Evar n)          << st_modify (addUse n . incCpx . checkFail n)            $ st
analyzeAst k (CaseAlt p e)     st = k (CaseAlt p e)     << st_modify (delUses (patNames p) . incCpx)              $ st
analyzeAst k (Qgen pats e)     st = k (Qgen pats e)     << st_modify (delUses (concatMap patNames pats) . incCpx) $ st
analyzeAst k (Qrecur p e1 e2)  st = k (Qrecur p e1 e2)  << st_modify (delUses (patNames p) . incCpx)              $ st
analyzeAst k (Pctor n vars)    st = k (Pctor n vars)    << st_modify (addUse n . incCpx)                          $ st
analyzeAst k (Tname n args)    st = k (Tname n args)    << st_modify (addTUse n)                                  $ st

|| handle extra info and updates around rewriting defns
analyzeAst k a st
    = k a >>= update $ st', if isDefn a
    = k a . incCpx   $ st,  otherwise
      where
        loc    = defnLocInfo a
        inCpx  = view azCpx st  || capture current complexity and
        inUses = view azUses st || use values to add back in later
        st'    = (over azLoc (mergeLocInfo loc) . clrCpxUses) st

        || remove variable uses, and update cpx and free info in the defn
        update d st
            = (d', st2)
              where
                n     = defnName d
                st1   = delUses (concatMap patNames (fnVars d)) st, if isDefFn d
                      = st,                                         otherwise
                cpx   = view azCpx st1
                uses  = view azUses st1
                free  = b_deleteKey cmpname n uses, if isDefAbs d       || remove all def name from abstype free refs
                      = b_delete cmpname n uses,    if isDefType d      || remove def name (Tname) occurrence, since it is not a free ref
                      = uses,                       otherwise
                d'    = (set defnAnCpx cpx . set defnAnFree (b_keysSet free)) d
                st2   = (set azCpx (inCpx + cpx + 1) . set azUses (b_union cmpname inUses free)) st1


|| internal version of analyze, used by analyze and analyzeWarn
|| tally complexity, free variables, and use counts for each defn and update their annos
analyze' :: globalEnv -> (globalEnv, azSt)
analyze' genv
    = (genv', st)
      where
        m           = view genvMod genv
        defs        = view modDefns m
        (defs', st) = astRewriteMap (astRewriteLex analyzeAst) defs initAzSt
        genv'       = over genvMod (set modDefns defs') genv

|| run analysis, and report any unused variable and non-exhaustive pattern warnings
analyzeWarn :: globalEnv -> excpt globalEnv
analyzeWarn genv
    = ex_return genv' warns, if null errs
    = ex_errs errs,          otherwise
      where
        (genv', st)   = analyze' genv
        (errs, warns) = partition isError $ view azErrs st

|| run analysis, ignoring any unused variable warnings
analyze :: globalEnv -> excpt globalEnv
analyze genv
    = ex_return genv' []
      where
        genv' = fst $ analyze' genv

|| run analysis on a defnMap, but don't return any exceptions
analyzeDefns :: defnMap -> globalEnv -> defnMap
analyzeDefns defs genv
    = fst . astRewriteMap (astRewriteLex analyzeAst) defs $ initAzSt

|| run analysis on a single expr, collecting only the cpx info
analyzeExprCpx :: expr -> globalEnv -> int
analyzeExprCpx e genv
    = view azCpx . snd . astRewriteLex analyzeAst e $ initAzSt

|| run analysis on a single expr, collecting only the free variable info
analyzeExprFree :: expr -> globalEnv -> s_set name
analyzeExprFree e genv
    = b_keysSet . view azUses . snd . astRewriteLex analyzeAst e $ initAzSt
