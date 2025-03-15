|| stg.m -- convert AST to Spineless Tagless G-Machine VM representation, using eval-apply model


%export codeObj codePtr stgCode heapInfo heapObj heapPtr stgHeap stackObj stgStack continuation
        value refSpace ref r0Ref r1Ref r2Ref isReg opr r0Opr r1Opr r2Opr applyToEnvPtr enterEnv0Ptr selRetPtr
        selApplyPtr blackHolePtr localEnv stgInsn showvalue cmpopr showopr showstgInsn showheapObj showstackObj
        buildStg heapObjWords stringWords

%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>                 (>>=)/st_bind (<$>)/st_fmap (<<)/st_left (>>)/st_right
%import "analyze"
%import "ast"
%import "dependency"
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"


|| a code object is a locInfo and a list of STG instructions
codeObj  == (locInfo, [stgInsn])                || source fn name and loc info, stg code
codePtr  == int                                 || index into the stgCode map
stgCode  == (m_map codePtr codeObj, codePtr)    || code map, next unused codePtr value

|| lenses for codeObj
coLoc   = lensFst
coInsns = lensSnd


|| a heap object is a list of environment values tagged with a heap Info containing info for execution and garbage collection
heapInfo ::=
    || these heapObjects can be applied directly if the arities match the #args
    Hfun    codePtr! int! |       || function with arity int, list values are free variable values of the function
    Hthunk  codePtr!      |       || thunk (arity 0)
    Hfwd    heapPtr!      |       || forward to another heapPtr (used when an Hthunk is updated to its computed value)

    || these heapObjects need a more general helper function to apply
    Hpap    heapPtr!      |       || partial application of Hfun at heapPtr , list values are the under-applied arguments
    Hfbalt  codePtr! int! |       || fatbar alternate, with captured stack pointer to unwind to

    || these heapObjects are data only (no codePtr)
    Hstream [char]        |       || a byteStream holding a packed list of chars
    Harray  int!                  || a contiguous array of size int, starting with the next word

heapObj     == (heapInfo, [value])
closure     == heapObj                          || type synonym for a heapObj
closureInfo == (codePtr, int, bool, bool)       || codePtr, arity, free vars empty, isDefFun
heapPtr     == int                              || index into the stgHeap map
stgHeap     == (m_map heapPtr heapObj, heapPtr) || heap map, next unused heapPtr value (and top of statically-allocated heap)

|| compute the number of 8-byte words each heap object requires, including the tag/info word
heapObjWords :: heapObj -> int
heapObjWords (info, env)
    = hwords info + #env
      where
        hwords (Hstream s) = stringWords s + 1  || tag word plus words for string
        hwords (Harray n)  = n + 1              || tag word plus n words for the values
        hwords info        = 2                  || all other heapObjs contain a pointer plus the tag word

|| compute the number of 8-byte words each packed string requires
stringWords :: string -> int
stringWords s
    = len $div 8 + extra
      where
        len   = #s
        extra = 1, if len $mod 8 > 0
              = 0, otherwise

|| a stack object is a continuation codePtr and a list of values (free variable values for the continuation code)
stackObj     == (codePtr, [value])
continuation == stackObj         || type synonym for stackObj
stgStack     == [continuation]

|| values are either unboxed Words, code pointers, or heap pointers
value ::= Word int! | Caddr codePtr! | Haddr heapPtr!

|| a ref represents a variable holding a value, and is an index into a particular space
refSpace ::=
    Arg |       || incoming arguments vector
    Env |       || free variable environment vector in the current closure
    Frm |       || a frame pointer to a contiguous region of heap space
    Reg         || registers vector holding special values and temps:
                || Reg 0 holds returned tag / value in case evaluations
                || Reg 1 holds arity of args (from arity of returned ctor, used when having to create a Pvar binding to the entire returned ctor)
                || Reg 2 holds heapPtr of current closure
                || Reg 3 .. are general registers

ref == (refSpace, int)

isReg :: ref -> bool
isReg (Reg, n) = True
isReg r        = False

|| special reg refs
r0Ref, r1Ref, r2Ref, e0Ref :: ref
r0Ref = (Reg, 0)        || tag of ctor or word value when returning to a context closure
r1Ref = (Reg, 1)        || arity of ctor when returning to a context closure (needed for Vclosure & Update to capture correct number of args)
r2Ref = (Reg, 2)        || heapPtr of owning closure (for updates and closure env references)
e0Ref = (Env, 0)

|| an instruction operand is either an immediate value or a ref
opr ::= Imm value | Ref ref

|| special reg oprs
r0Opr, r1Opr, r2Opr, e0Opr :: opr
r0Opr = Ref r0Ref
r1Opr = Ref r1Ref
r2Opr = Ref r2Ref
e0Opr = Ref e0Ref

|| make an opr from an integer
makeImmOpr :: int -> opr
makeImmOpr n = Imm (Word n)

|| STG instructions, chosen as a balance between lowering the representation closer to native code, and efficient interpretation
stgInsn ::=

    || control flow
    Call        codePtr [opr]           |    || tail call known codePtr (no free vars), copying [opr] .. to args
    Enter       heapPtr [opr]           |    || enter known closure hp with arguments [opr ..]
    EnterInd    opr     [opr]           |    || enter known closure (indirectly referencing hp from opr0) with arguments [opr1..]
    Apply       opr     [opr]           |    || apply unknown closure [opr0] to arguments [opr1 ..], may be over- or under-saturated
    ApplyToEnv                          |    || apply unknown closure [Reg 0] to arguments [env], may be over- or under-saturated
    Fail        opr                     |    || opr points to an Hfbalt; unwind the stack to the saved stack and enter the Hfbalt closure
    Ret         opr     opr             |    || copy [opr0,1] to Reg 0,1, pop a continuation from the stack, and enter it with current args vector
    RetEnv                              |    || copy env to Reg 0,1, args[0 .. #env-1], pop a continuation closure from the stack, and enter it
    CaseVec     opr     [codePtr]       |    || use [opr] as an index into the codePtrs and jump to that codePtr
    Jmp                 codePtr         |    || unconditional jump to codePtr
    Jeq         opr opr codePtr         |    || compare [opr0] with [opr1], and jump to codePtr if ==
    Jlt         opr opr codePtr         |    || compare [opr0] with [opr1], and jump to codePtr if <

    || stack manipulation
    PushCont    codePtr [opr]           |    || push a continuation closure on the stack with codePtr and environment [[opr0] ..]
    PopEnv      int                     |    || pop N values from the stack into the environment for a continuation closure

    || heap manipulation
    AllocFrame  int                     |    || allocate int contiguous heapPtrs and store the base heapPtr in the framebase register
    Fclosure    ref codePtr int [opr]   |    || ref := addr of new Hfun closure
    Tclosure    ref codePtr [opr]       |    || ref := addr of new Hthunk closure
    Sclosure    ref codePtr [opr]       |    || ref := addr of new Hfbalt closure
    Vclosure    ref codePtr             |    || ref := addr of new Hthunk closure, with env = r0, r1, args[0..r1-1]
    Update      opr opr                 |    || update the closure pointed to by opr0 to the codePtr referenced by opr1,
                                             || with Reg 0, Reg 1 and current args vector as the environment vars

    || primitive operations on words
    Mov         ref opr                 |    || ref := [opr1]
    Add         ref opr opr             |    || ref := [opr1] +    [opr2]
    Sub         ref opr opr             |    || ref := [opr1] -    [opr2]
    Mul         ref opr opr             |    || ref := [opr1] *    [opr2]
    Div         ref opr opr             |    || ref := [opr1] $div [opr2]
    Mod         ref opr opr             |    || ref := [opr1] $mod [opr2]
    Cmp         ref opr opr             |    || ref := [opr1] $cmp [opr2]   -- returns 0# -> EQ, 1# -> LT, 2# -> GT
    Sel         ref opr                 |    || ref := args[opr] -- the value in opr is a word which is used to index into the args vector

    || primitive bit-wise operations on words
    Band        ref opr opr             |    || ref := [opr1] .&. [opr2]
    Bor         ref opr opr             |    || ref := [opr1] .|. [opr2]
    Bxor        ref opr opr             |    || ref := [opr1] xor [opr2]
    Bnot        ref opr                 |    || ref := complement [opr1]
    Bshl        ref opr opr             |    || ref := [opr1] .<<. [opr2]
    Bshr        ref opr opr             |    || ref := [opr1] .>>. [opr2]

    || primitive operations on byte streams
    AllocStream ref opr opr             |    || ref := new byteStream ref with fd field opr0 and size opr1
    ReadStream  ref opr                 |    || ref := read a byte from the byteStream ref in opr1
    WriteStream ref opr opr             |    || write byte in opr1 to the byteStream ref in opr0, returning success/fail indication in ref

    || primitive operations on arrays
    AllocArray  ref opr                 |    || ref := new array ref for an array of size [opr]
    FillArray   opr opr                 |    || fill array [opr1] with value [opr2]
    CopyArray   opr opr                 |    || copy contents of array [opr1] to array [opr2] (sizes assumed to be the same!)
    ReadArray   ref opr opr             |    || ref := value from the array ref [opr1] at index [opr2]
    WriteArray  opr opr opr             |    || write value [opr3] to array ref [opr1] at index [opr2]

    || primitive system operations           || 0: exit
                                             || 1: openFileRead
                                             || 2: openFileWrite
                                             || 3: openFileAppend
                                             || 4: closeFile
                                             || 5: readFile
                                             || 6: writeFile
                                             || 7: mtimeFile
                                             || 8: getArg
                                             || 9: systemCmd
    SystemOp    int ref opr


|| state passed through buildStg:
|| dynamic state:
|| genv:      global environment
|| code:      stgCode
|| heap:      stgHeap
||
|| lexically-scoped state:
|| lenv:      local environment, mapping a name to an opr (an Imm value, heap ptr for top-level defns, or a ref for binding locations)
|| lcls:      local closure info, mapping a name to a closure's codePtr and arity for use in directly calling/entering a closure
|| loc:       locInfo
|| regGen:    next temp reg to allocate
|| frmGen:    next frame slot to allocate
|| builtin:   currently evaluating a builtin (used in caseAlt code generation optimization of Pvar)
||            Note: this is a state variable because it would be more difficult to pass this little-used boolean thoroughout the
||            buildCode functions; it must be set correctly lexically any time we are building caseAlts
localEnv == m_map name opr
localCls == m_map name closureInfo
bdDynSt  == (globalEnv, stgCode, stgHeap)
bdLexSt  == (localEnv, localCls, locInfo, int, int, bool)
bdSt     == (bdDynSt, bdLexSt)

|| lenses for bdSt
bdDyn       = lensFst
bdLex       = lensSnd
bdGenv      = composeLens bdDyn lensTup3_0
bdCode      = composeLens bdDyn lensTup3_1
bdHeap      = composeLens bdDyn lensTup3_2
bdLenv      = composeLens bdLex lensTup6_0
bdLcls      = composeLens bdLex lensTup6_1
bdLoc       = composeLens bdLex lensTup6_2
bdRegGen    = composeLens bdLex lensTup6_3
bdFrmGen    = composeLens bdLex lensTup6_4
bdBuiltin   = composeLens bdLex lensTup6_5
bdCodeMap   = composeLens bdCode lensFst
bdCodePtr   = composeLens bdCode lensSnd
bdHeapMap   = composeLens bdHeap lensFst
bdHeapPtr   = composeLens bdHeap lensSnd

|| lift lens operations into bdSt
bd_view :: lens bdSt * -> state bdSt *
bd_view lns st = (view lns st, st)

bd_set :: lens bdSt * -> * -> state bdSt ()
bd_set lns v st = ((), set lns v st)

bd_over :: lens bdSt * -> (* -> *) -> state bdSt ()
bd_over lns f st = ((), over lns f st)

|| initial build state
|| fixed code pointers for special generic operations, must ensure they match the order of the special routines in initBdSt
unpackPtr, updatePtr, selRetPtr, selApplyPtr :: codePtr
unpackPtr     = 0      || generic code to unpack an evaluated thunk into its tag (Reg 0), arity (Reg 1) and args (Arg 0 ..) from the thunk's env
updatePtr     = 1      || generic code to update a thunk to return its evaluated value
applyToEnvPtr = 2      || generic code to apply [r0] to env, used to apply additional args to a partial application
enterEnv0Ptr  = 3      || generic code to perform an enter on env0, used to forward an updated thunk to the thunk holding its new value
selRetPtr     = 4      || generic code to perform a select on constructor arguments and return the corresponding word value
selApplyPtr   = 5      || generic code to perform a select on constructor arguments and apply the corresponding closure
blackHolePtr  = 6      || fixed location for the blackhole update "trampoline" for thunks

|| init resource allocation counters
|| initialize the regGen to 3 (0 - 2 are reserved) and the frmGen to 0
initResources, initFrame :: bdSt -> bdSt
initResources = set bdRegGen 3 . set bdFrmGen 0
initFrame     = set bdFrmGen 0

initBdSt :: globalEnv -> bdSt
initBdSt genv
    = initResources ((genv, code, heap), (m_empty, m_empty, emptyLocInfo, 0, 0, False))
      where
        code     = (m_fromList cmpcodePtr special, #special)    || code has special generic routines pre-installed
        heap     = (m_empty, 0)
        special  = [ (unpackPtr,     (builtinInfo "<unpack>",     [RetEnv]))                                                || generic unpack
                   , (updatePtr,     (builtinInfo "<update>",     [Update e0Opr (Imm (Caddr unpackPtr)), Ret r0Opr r1Opr])) || generic update
                   , (applyToEnvPtr, (builtinInfo "<applytoenv>", [ApplyToEnv]))                                            || generic applyToEnv
                   , (enterEnv0Ptr,  (builtinInfo "<enterenv0>",  [EnterInd e0Opr []]))                                     || generic enter env0
                   , (selRetPtr,     (builtinInfo "<selret>",     [Sel r0Ref (Ref (Env, 0)), Ret r0Opr (makeImmOpr 0)]))    || generic sel for a word value
                   , (selApplyPtr,   (builtinInfo "<selapply>",   [Sel r0Ref (Ref (Env, 0)), Apply r0Opr []]))              || generic sel for a thunk value
                   , (blackHolePtr,  (builtinInfo "<blackhole>",  [Jmp blackHolePtr]))                                      || blackhole update "trampoline"
                   ]
        builtinInfo s = pathOnlyLocInfo (Builtin s)

|| alloc a int resource in bdSt strictly
allocResource :: lens bdSt int -> int -> state bdSt int
allocResource lns n
    = bd_view lns >>= inc
      where
        inc r
            = r' $seq bd_set lns r' >> st_pure r
              where r' = r + n

|| alloc space in the stgCode, to be updated later
allocCode :: state bdSt codePtr
allocCode = allocResource bdCodePtr 1

|| set stgCode[codePtr] to codeObj
updateCode :: codePtr -> codeObj -> state bdSt codePtr
updateCode cp obj
    = bd_over bdCodeMap (m_insert cmpcodePtr cp obj) >> st_pure cp

|| make a codeObj from a list of stgInsns
makeCode :: [stgInsn] -> state bdSt codePtr
makeCode ins
    = st_bind2 (bd_view bdLoc) allocCode mkObj
      where
        mkObj loc cp = updateCode cp (loc, ins)

|| alloc space in the stgHeap, to be updated later
allocHeap :: int -> state bdSt heapPtr
allocHeap = allocResource bdHeapPtr

|| set stgHeap[heapPtr] to heapObj
updateHeap :: heapPtr -> heapObj ->state bdSt heapPtr
updateHeap hp obj st = (hp, over bdHeapMap (m_insert cmpheapPtr hp obj) st)

lookupHeap :: heapPtr -> state bdSt heapObj
lookupHeap hp st = (fromJust . m_lookup cmpheapPtr hp . view bdHeapMap $ st, st)

|| lookup possible closureInfo for an expr
lookupClosureInfo :: expr -> state bdSt (maybe closureInfo)
lookupClosureInfo (Evar n) st = (m_lookup cmpname n (view bdLcls st), st)
lookupClosureInfo e        st = (Nothing, st)

|| add possible arity info for variable names of a defn, using a new closureInfo
|| We can sometimes infer the arity to use for each of a defn's vars when evaluating them, based upon the
|| corresponding portion of the defn's DefSpec (if present). This allows a var to be evaluated with a
|| cheaper EnterInd, rather than a general Apply.  The arity is known to be 0 if the DefSpec portion for that
|| var is a value type (not a Tarr fn type) and the baseType is not a Tvar (universally qualified unknown type).
|| Otherwise, due to currying and how definitions can be written with fewer vars than the spec implies, the arity
|| can't be determined.
addVarArityInfo :: name -> [pat] -> state bdSt ()
addVarArityInfo dn vars
    = bd_view bdGenv >>= go
      where
        go genv = addInfo $ lookupSpec genv dn

        addInfo Nothing = st_pure ()                            || no DefSpec for the defn name, can't infer arity
        addInfo (Just spec)
            = st_mapM_ insInfo . zip2 vars . map typeList $ ts  || check each var with its corresponding type info from the DefSpec
              where
                ts = fst . typeList . view defnType $ spec

        || insert a dummy closureInfo for the arg n with inferred arity 0, if it is a value type that isn't universally qualified (Tvar)
        insInfo (Pvar n, ([], bt))
            = bd_over bdLcls (m_insert cmpname n (0, 0, False, False)), if ~isTvar bt

        insInfo _ = st_pure ()

|| make an operand from an expr
makeOpr :: expr -> state bdSt opr
makeOpr (Evar n)      = (fromJust . m_lookup cmpname n) <$> bd_view bdLenv      || lookup a variable reference in the localEnv
makeOpr (EprimInt n)  = st_pure . Imm . Word $ n                                || immediate int value
makeOpr (EprimChar c) = st_pure . Imm . Word . code $ c                         || immediate char value

|| create a byteStream heap object for primitive strings
makeOpr (EprimString s)
    = allocHeap (stringWords s + 1) >>= mkByteStream
      where
        mkByteStream hp = updateHeap hp (Hstream s, []) >> (st_pure . Imm . Haddr) hp

makeOpr e = error ("makeOpr: unhandled expr: " ++ showAstRaw e)

|| allocate a reg
allocReg :: state bdSt ref
allocReg = pair Reg <$> allocResource bdRegGen 1

|| allocate a contiguous region of frame slots, adding 2 words to
|| account for the tag word and the code ptr in each Hfun/Hthunk heap object
allocFrmSlots :: int -> state bdSt ref
allocFrmSlots n = pair Frm <$> allocResource bdFrmGen (n + 2)

|| bind a Pvar name to a newly-allocated reg ref
makeRef :: pat -> state bdSt ref
makeRef (Pvar n) st
    = (r, st2)
      where
        (r, st1) = allocReg st
        st2      = over bdLenv (m_insert cmpname n (Ref r)) st1

makeRef _ _ = error "makeRef: non-Pvar"


|| run a build function in a new lexical environment of Arg and Env refs,
|| restoring the environment afterwards. The first argument specifies the
|| lens to use to focus in on a particular lexical state variable to save/restore.
|| The second argument is a pattern list, which allows Pvars as well as Pwildcards,
|| so that only the Pvars are bound to their corresponding arg ref numbers.
inLexEnv :: lens bdSt * -> [pat] -> [name] -> state bdSt ** -> state bdSt **
inLexEnv lns pvs fns ma
    = bd_view lns >>= newLex
      where
        || insert only Pvars with their corresponding Arg ref
        insVars (Pvar v, n) xs = (v, Ref . pair Arg $ n) : xs
        insVars p           xs = xs

        addEntries es m   = foldl addEntry m es
        addEntry m (k, v) = m_insert cmpname k v m

        newLex lex
            = bd_over bdLenv (addEntries (arefs ++ erefs)) >> ma << bd_set lns lex
              where
                arefs = foldr insVars [] (zip2 pvs [0 ..])
                erefs = zip2 fns (map (Ref . pair Env) [0 ..])

|| run a build function in a new environment for a defn or other expr (continuation, alt for fatbar)
|| saving and restoring the whole lexical state
inDefEnv :: locInfo -> [pat] -> [name] -> state bdSt [stgInsn] -> state bdSt [stgInsn]
inDefEnv loc pvs fns ma
    = inLexEnv bdLex pvs fns mb
      where
        mb = st_modify initResources          >> || initialize both FrmGen and RegGen for new Defn context
             bd_over bdLoc (mergeLocInfo loc) >>
             ma

|| run a build function in a new environment of Arg refs for a caseAlt, saving and restoring the whole
|| lexical state. A common Env for all free vars in all CaseAlts in the Ecase has been set up at the
|| beginning of the continuation closure
inAltEnv :: [pat] -> state bdSt [stgInsn] -> state bdSt [stgInsn]
inAltEnv pvs ma
    = inLexEnv bdLex pvs [] mb
      where
        mb = st_modify initFrame >> ma  || initialize the FrmGen, but not the RegGen (maintain registers still alive)


|| build the code for a caseAlt
buildCodeAlt :: caseAlt -> state bdSt [stgInsn]

|| bind the Pvar name to an opr, either r0 if we are building for a builtin result,
|| or a new reg operand for a Vclosure that captures the current r0, r1, and args and unpacks them when called
|| Note: this code uses the bdBuiltin state var to determine if we are currently working inside a builtin; this
|| must be set lexically whenever we enter a new Ecase
buildCodeAlt (CaseAlt (Pvar n) e)
    = st_pure [Ret r0Opr r1Opr],        if _eq cmpexpr e (Evar n)  || just return existing r0/r1/args
    = bd_view bdBuiltin >>= doAlt, otherwise
      where
        doAlt inBuiltin
            = inAltEnv [] (buildCode e),   if inBuiltin || the Pvar n has already been bound to an allocated register for builtins
            = allocReg >>= createAlt, otherwise
              where
                createAlt reg
                  = addClosure <$> inAltEnv [] (bd_over bdLenv (m_insert cmpname n (Ref reg)) >> buildCode e)
                    where
                      addClosure eins = Vclosure reg unpackPtr : eins

buildCodeAlt (CaseAlt (Pctor cn vars) e)
    = inAltEnv vars (addVarArityInfo cn vars >> buildCode e)

buildCodeAlt (CaseAlt p e)
    = inAltEnv [] (buildCode e)

buildCodeAlt _ = undef  || added to remove compiler warning (all caseAlts handled, but it is a type alias for ast)

|| build the code for the CaseAlts, resetting the regGen and frmGen values to their common initial values for each,
|| setting them to the max regGen and frmGen values used upon return
buildCodeAltsTally :: [caseAlt] -> state bdSt [[stgInsn]]
buildCodeAltsTally alts
    = st_bind2 (bd_view bdRegGen) (bd_view bdFrmGen) doAlts
      where
        doAlts brc bfc
            = foldr go (st_pure ([], brc, bfc)) alts >>= setMax
              where
                go alt k
                    = bd_set bdRegGen brc >> bd_set bdFrmGen bfc >> buildCodeAlt alt >>= getCounts
                      where
                        getCounts ins
                            = st_bind3 (bd_view bdRegGen) (bd_view bdFrmGen) k doMax
                              where
                                doMax rc fc (bbs, mrc, mfc) = st_pure (ins : bbs, max2 cmpint mrc rc, max2 cmpint mfc fc)

                setMax (bbs, mrc, mfc)
                    = bd_set bdRegGen mrc >> bd_set bdFrmGen mfc >> st_pure bbs

|| convert a list of CaseAlts to the stgInsns to match against them,
|| based upon the caseSel of the Ecase and the tag opr, and return the code
|| note that the alts all share a common context with the same env (union of all free vars)
buildCodeAlts :: caseSel -> opr -> [caseAlt] -> state bdSt [stgInsn]

buildCodeAlts Cnone  topr alts  = error "buildCodeAlts: caseSel type is unspecified"
buildCodeAlts Csingle topr alts = buildCodeAlt $ hd alts

buildCodeAlts Cvectored topr alts
    = buildCodeAltsTally alts >>= st_mapM makeCode >>= mkVec
      where
        mkVec cps = st_pure [CaseVec topr cps]

|| create a sequence of Jeq topr <tag> <codeptr> insns up to the first
|| irrefutable alt, then fall through to its insns
buildCodeAlts Cpriority topr alts
    = st_bind2 (bd_view bdGenv) (buildCodeAltsTally alts) go
      where
        go genv bbs
            = foldr doAlt (st_pure [Jmp blackHolePtr]) (zip2 alts bbs)  || the Jmp blackHolePtr should never be encountered,
              where                                                     || but it is an easy insn to have as "bottom"
                doAlt (a, bb) k
                  = st_pure bb,          if isPvarOrWild p              || irrefutable (unconditional) case; early-outs
                  = st_liftA2 (:) tst k, otherwise                      || refutable (conditional) case; add Jeq of tag
                    where
                      CaseAlt p _ = a
                      tst         = Jeq topr (mkTagOpr p) <$> makeCode bb

                mkTagOpr (Plit (EprimInt n))  = makeImmOpr n
                mkTagOpr (Plit (EprimChar c)) = makeImmOpr (code c)
                mkTagOpr (Pctor cn vars)      = makeImmOpr . fromMaybe 0 . elemIndex cmpname cn . fromMaybe [] . lookupCtorNames genv $ cn
                mkTagOpr p                    = topr

|| build the alts and the code to select from them in the context of a new
|| continuation closure, then return a union of all the free vars and a new
|| codePtr with the insns.  Also adds a PopEnv insn to the beginning of the continuation
|| closure to determine how many free env variables were pushed onto the stack to pop into the Env
buildCodeAltsCont :: caseSel -> [caseAlt] -> state bdSt ([name], codePtr)
buildCodeAltsCont sel alts
    = st_bind3 (bd_view bdGenv) (bd_view bdLoc) allocCode mkCont
      where
        mkCont genv loc cp
              || note: emptyLocInfo used to prevent concatenation of same defn name multiple times in codeObj locInfo
            = inDefEnv emptyLocInfo [] fns (buildCodeAlts sel r0Opr alts) >>= updateObj
              where
                fns           = filter isInternal . s_toList . foldl (s_union cmpname) s_empty . map (converse analyzeExprFree genv) $ alts
                updateObj ins = st_pure (fns, cp) << updateCode cp (loc, PopEnv (#fns) : ins)

|| build the alts and the code to select from them in the current context, returning the insns along
|| with a list of Mov insns which will move any existing Frm refs to new registers if there are any refs
|| to them in the free vars, and any of the case alts allocate a frame (which would destroy the current Frm references)
buildCodeAltsNoCont :: caseSel -> opr -> [caseAlt] -> state bdSt ([stgInsn], [stgInsn])
buildCodeAltsNoCont sel topr alts
    = getFrmRefs >>= mkAlts >>= mkMovs
      where

        || get any existing Frm refs in the alts, if any alt needs to allocate a new frame
        getFrmRefs
            = st_bind3 (bd_view bdGenv) (bd_view bdLenv) (bd_view bdFrmGen) getRefs
              where
                getRefs genv lenv frc
                    = zip2 frs <$> st_mapM (const allocReg) frs    || allocate a new reg for each Frm ref that needs to be moved
                      where
                        fns  = filter isInternal . s_toList . foldl (s_union cmpname) s_empty . map (converse analyzeExprFree genv) $ alts
                        frs  = catMaybes . map getFrmRef $ fns, if frc > 0 & any needsFrm alts    || collect any existing Frm refs
                             = [],                              otherwise                         || no moves of existing Frm refs required

                        || lookup n in localEnv, returning the opr and name if it is an Frm ref
                        getFrmRef n
                            = m_lookup cmpname n lenv $mb_bind check
                              where
                                isFrm (Ref (Frm, x)) = True
                                isFrm o              = False

                                check o
                                    = Just (o, n), if isFrm o
                                    = Nothing,     otherwise

                needsFrm (CaseAlt _ (Elet _ _ _ _))  = True        || caseAlt has an Elet, which allocs Frm slots
                needsFrm _                           = False

        || build the alts in the existing Env, adding bindings for any moved Frm refs
        mkAlts binfo
           = pair binfo <$> inAltEnv [] (st_mapM_ addBinding binfo >> buildCodeAlts sel topr alts)
             where
               addBinding ((_, n), r) = bd_over bdLenv (m_insert cmpname n (Ref r))

        || add the Mov insns for any moved Frm refs
        mkMovs (binfo, ins)
            = st_pure (map mkMov binfo, ins)
              where
                mkMov ((o, _), r) = Mov r o

|| build the code to handle a call to a builtin function; these will all be single stgInsns
|| the destination ref to use is passed in, as it can be optimized to a new Reg ref in
|| certain cases (see buildCode for Ecase)
buildCodeCallBuiltin :: name -> [expr] -> ref -> state bdSt stgInsn
buildCodeCallBuiltin n args dst
    = go (getName n)
      where
        opr0      = makeOpr (args ! 0)
        opr1      = makeOpr (args ! 1)
        opr2      = makeOpr (args ! 2)
        zero      = makeImmOpr 0

        opD0  op    = st_pure (op dst r0Opr)
        opDS  op    = op dst       <$>   opr0
        opDZS op    = op dst zero  <$>   opr0
        opDSS op    = st_liftA2 (op dst) opr0 opr1
        opSS  op    = st_liftA2 op       opr0 opr1
        opSSS op    = st_liftA3 op       opr0 opr1 opr2

        || primitive arithmetic operations on word#
        go "+#"               = opDSS Add
        go "-#"               = opDSS Sub
        go "*#"               = opDSS Mul
        go "div#"             = opDSS Div
        go "mod#"             = opDSS Mod
        go "cmp#"             = opDSS Cmp
        go "getTag#"          = opD0  Mov

        || primitive bit operations on word#
        go "band#"            = opDSS Band
        go "bor#"             = opDSS Bor
        go "bxor#"            = opDSS Bxor
        go "bnot#"            = opDS  Bnot
        go "bshl#"            = opDSS Bshl
        go "bshr#"            = opDSS Bshr

        || operations on packed byte streams
        go "allocByteStream#" = opDZS AllocStream
        go "allocFileStream#" = opDSS AllocStream       || a FileStream uses the same Hstream obj as a ByteStream, only with the fd field specified
        go "readByteStream#"  = opDS  ReadStream 
        go "writeByteStream#" = opDSS WriteStream

        || operations on arrays
        go "allocArray#"      = opDS  AllocArray
        go "fillArray#"       = opSS  FillArray
        go "copyArray#"       = opSS  CopyArray
        go "readArray#"       = opDSS ReadArray
        go "writeArray#"      = opSSS WriteArray

        || system and file operations
        go "exit#"            = opDS  (SystemOp 0)
        go "openFileRead#"    = opDS  (SystemOp 1)
        go "openFileWrite#"   = opDS  (SystemOp 2)
        go "openFileAppend#"  = opDS  (SystemOp 3)
        go "closeFile#"       = opDS  (SystemOp 4)
        go "readFile#"        = opDS  (SystemOp 5)
        go "writeFile#"       = opDS  (SystemOp 6)
        go "mtimeFile#"       = opDS  (SystemOp 7)
        go "getArg#"          = opDS  (SystemOp 8)
        go "systemCmd#"       = opDS  (SystemOp 9)

        go s                  = error ("buildCodeCallBuiltin: missing definition for builtin " ++ s)


|| build code for an Ecall (function) or an Evar (thunk), based upon info from the call fopr and any closureInfo
buildCodeCall :: expr -> [expr] -> state bdSt [stgInsn]

buildCodeCall (Evar f) args
    = singleton <$> buildCodeCallBuiltin f args r0Ref, if isBuiltin f & ~isConstructor f

buildCodeCall fe args
    = st_bind3 (makeOpr fe) (st_mapM makeOpr args) (lookupClosureInfo fe) mkCall
      where
        mkCall fopr aoprs minfo
            = checkImm fopr
              where
                noprs = #aoprs

                || check the fopr for an immediate address (known code/heap ptr)
                checkImm (Imm (Caddr cp)) = st_pure [Call cp aoprs]
                checkImm (Imm (Haddr hp)) = lookupHeap hp >>= checkHobj hp
                checkImm _                = checkInfo minfo

                || check for closureInfo associated with the fopr
                checkInfo Nothing                      = mkGeneral
                checkInfo (Just (cp, arity, ef, isFn)) = mkDirect Nothing cp arity ef isFn

                || use the known heapObj info 
                checkHobj hp (Hfun cp arity, env) = mkDirect (Just hp) cp arity (null env) True
                checkHobj hp (Hthunk cp, env)     = mkDirect (Just hp) cp 0     (null env) False
                checkHobj hp hObj                 = mkGeneral

                || use known codePtr and arity to check for over/under application and choose call form
                mkDirect mhp cp arity emptyEnv isFn
                    = st_pure [Ret fopr (makeImmOpr 0)],    if noprs == 0 & arity > 0   || un-applied fn evaluates to self
                    = mkOverApp,                            if noprs > arity            || over-applied fn split in two
                    = st_pure [Apply fopr aoprs],           if noprs < arity            || under-applied fn handled at runtime with Apply
                    = st_pure [Call cp aoprs],              if isFn & emptyEnv          || can call cp directly if it is a fn with empty env
                    = st_pure [Enter (fromJust mhp) aoprs], if isJust mhp               || can enter a closure directly if heapPtr known
                    = st_pure [EnterInd fopr aoprs],        otherwise                   || enter closure indirectly from fopr
                      where
                        (aoprs1, aoprs2)  = splitAt arity aoprs
                        mkOverApp         = (PushCont applyToEnvPtr aoprs2 :) <$> mkCall fopr aoprs1 minfo

                || unknown closure; make a general Apply
                || Note: even if we are calling with null aoprs (nominally a Thunk that could be EnterInd),
                || we don't know the arity; it might have to return a partial application, so we always use Apply
                mkGeneral = st_pure [Apply fopr aoprs]


|| convert an expr to a list of stgInsns that will evaluate that expr at runtime
buildCode :: expr -> state bdSt [stgInsn]

buildCode (EprimInt n)  = st_pure [Ret (makeImmOpr n) (makeImmOpr 0)]
buildCode (EprimChar c) = st_pure [Ret (makeImmOpr n) (makeImmOpr 0)] where n = code c

|| convert an Evar reference to code to evaluate a thunk or self-evaluate a value
buildCode (Evar n) = buildCodeCall (Evar n) []

|| convert an Elet into code/heap bindings for its defns, insns to make Hfun / Hthunk closures at runtime
|| for those bindings, and the insns for the Elet's expr
buildCode (Elet rec defs x e)
    = bd_view bdFrmGen >>= fFrm || get the current frm count to determine if we need to add an AllocFrame insn or not
      where
        defl = m_toList defs

        || defs converted in parallel in two phases: allocate & bind, then create and update,
        || so that mutually-recursive references can lookup their values in the bdLenv
        fFrm fg = st_mapM allocObj defl >>= st_mapM createDefnCode >>= createExprCode fg e

        || allocate a codePtr and a frame slot ref, and insert the ref and closure info into the lenv for the name n
        allocObj (n, d)
            = st_bind2 (allocFrmSlots nslots) allocCode mkObj
              where
                fns    = filter isInternal . s_toList . view defnAnFree $ d        || internal free var names of the defn
                nfns   = #fns
                nslots = nfns,               if isDefFn d
                       = max2 cmpint nfns 1, otherwise  || always allocate at least 1 slot for use by Update

                mkObj rf cp
                    = st_pure (d, cp, rf, fns)                     <<
                      bd_over bdLenv (m_insert cmpname n (Ref rf)) <<
                      bd_over bdLcls (m_insert cmpname n clsInfo)
                      where
                        clsInfo = (cp, fnArity d, null fns, isDefFn d)

        || create the code (stgInsns) and free var references for the defn and update the object's codePtr
        createDefnCode (d, cp, rf, fns)
            = st_bind3 (buildCode d) (st_mapM (makeOpr . Evar) fns) (bd_view bdLoc) updateObj
              where
                updateObj ins frs loc
                    = st_pure (d, cp, rf, frs) << updateCode cp (loc', ins)
                      where
                        loc' = mergeLocInfo (defnLocInfo d) loc

        || create the code for the expr, adding any Frm Mov insns and Fclosure/Tclosure instrs for the defns
        createExprCode fg e objs
            = buildCode e >>= addInsns >>= addFrame fg
              where
                mkClosure (d, cp, rf, frs)
                    = Fclosure rf cp (fnArity d) frs, if isDefFn d
                    = Tclosure rf cp frs,             otherwise

                addInsns ins = st_liftA2 pair (bd_view bdFrmGen) (st_pure $ map mkClosure objs ++ ins)

                || add the AllocFrame insn for a set of allocations (only add to the top-level of a set of nested
                || Elets by checking that the initial frmGen count is zero)
                addFrame fg1 (fg2, ins)
                    = st_pure (AllocFrame fg2 : ins), if fg1 == 0
                    = st_pure ins,                    otherwise

|| match case evaluation of (non constructor) builtin primitives to an stgInsn, and bind the result
|| to a newly-allocated ref. Evaluation of the scrutinee is a single instruction with word# operands
|| that doesn't require a continuation to capture free vars.  Also, builtin primitives
|| only return a single word# result (value or tag), so a single Pvar caseAlt is optimized
|| here to a new Reg allocation, rather than creating a new thunk closure to hold the result
buildCode (Ecase sel (Ecall (Evar fn) args) alts)
    = mkDst >>= mkCode, if isBuiltin fn & ~isConstructor fn
      where
        CaseAlt p e = hd alts
        singleVar   = _eq cmpcaseSel sel Csingle & isPvar p
        mkDst       = makeRef p, if  singleVar
                    = st_pure r0Ref, otherwise

        mkCode dst
            = st_liftA2 insMovs (buildCodeCallBuiltin fn args dst) buildAlts
              where
                buildAlts
                    = pair [] <$> buildCode e,                                                                    if singleVar
                    = inLexEnv bdBuiltin [] [] (bd_set bdBuiltin True >> buildCodeAltsNoCont sel (Ref dst) alts), otherwise

        insMovs bi (movs, ais) = movs ++ (bi : ais)

|| general case evaluation
buildCode (Ecase sel e alts)
    = st_bind2 (buildCode e) buildAlts mkCase
      where
        buildAlts = inLexEnv bdBuiltin [] [] (bd_set bdBuiltin False >> buildCodeAltsCont sel alts)

        mkCase eis (fns, cp)
            = mkCont <$> st_mapM (makeOpr . Evar) fns
              where
                mkCont fvs = PushCont cp fvs : eis

|| Special case for the polymorphic psuedo-function <pack>, which is represented as a Tname.
|| While it is nominally packing together args for a constructor, most of the time we are just
|| dynamically returning to a case continuation, so <pack> just becomes a Ret with the constructor's
|| tag in r0 and it's arity in r1, and the existing constructor args already in place in the args
|| array.  Actual packing of these in a new Hthunk heapObj is left to the Update instruction,
|| which allows thunks that don't require updates to leave it out.
buildCode (Tname f as)
    = st_pure [Ret (makeImmOpr tag) (makeImmOpr arity)], if _eq cmpname f builtinPackName
      where
        EprimInt tag   = as ! 1
        EprimInt arity = as ! 2

|| handle polymorphic pseudo-function <sel>, by pushing a sel continuation with the index
|| NOTE: if the inliner is allowed to inline constructors, the <pack> will get inlined to
|| an Ecall Tname like for <sel>, which is handled here, as well
buildCode (Ecall (Tname f as) args)
    = st_liftA2 mkSel (bd_view bdGenv) (buildCode e), if _eq cmpname f builtinSelName
    = buildCodeCall (Evar cn) args,                   otherwise
      where
        e          = args ! 0
        cn         = tformName (as ! 0)
        EprimInt i = as ! 1

        mkSel genv eis
            = PushCont selPtr [makeImmOpr i] : eis
              where
                t       = nthType i . view defnType . fromJust . lookupSpec genv $ cn   || get the individual types of the ctor
                selPtr  = selRetPtr,   if _eq cmptexpr t builtinTypeWord                || if we are selecting a word#, use Sel followed by a Ret
                        = selApplyPtr, otherwise                                        || otherwise it is a thunk; use Sel followed by an Apply

|| general case for Ecall: make oprs from the fn and the args,
|| then either generate an Apply or Enter or Call insn, depending
|| upon the lenv value of f
buildCode (Ecall f args) = buildCodeCall f args

|| build the alternate branch of an Efatbar in a new environment with all of its free var refs bound to the env of the Sclosure,
|| then build the primary branch with the fail var bound to a new Reg or Frm ref, and append it to the Sclosure insn
|| if the Efatbar is in a context that has Frm allocations, allocate space for the closure from the frame, otherwise allocate a
|| register to hold the closure
buildCode (Efatbar (Pvar n) e1 e2)
    = st_bind3 (bd_view bdGenv) (bd_view bdLoc) (bd_view bdFrmGen) mkAlt
      where
        mkAlt genv loc fg
            = st_bind4 allocClosure
                       allocCode
                       (st_mapM (makeOpr . Evar) fns)
                       (inDefEnv emptyLocInfo [] fns (buildCode e2))
                       updateObj
              where
                fns = filter isInternal . s_toList . analyzeExprFree e2 $ genv

                allocClosure
                    = allocReg,                 if fg == 0      || no existing Frame, so alloc to a reg
                    = allocFrmSlots $ #fns + 1, otherwise       || exisiting Frame, so just use that

                updateObj cls cp fvs ins
                    = updateCode cp (loc, ins) >>= mkPrim cls fvs

        mkPrim cls fvs altCp
            = mkClosure <$> inLexEnv bdLenv [] [] (bd_over bdLenv (m_insert cmpname n (Ref cls)) >> buildCode e1)
              where
                mkClosure insns = Sclosure cls altCp fvs : insns

|| an Efail invokes the Sclosure through the Fail insn
buildCode (Efail e)
    = mkFail <$> makeOpr e
      where
        mkFail fopr = [Fail fopr]

buildCode d
    = buildCodeDef d,                                       if isDefn d
    = error ("unhandled expr in buildCode: " ++ showAst d), otherwise
      where
        loc = defnLocInfo d

        buildCodeDef (DefFn n vars e an)
            = inDefEnv loc vars fns (addVarArityInfo n vars >> buildCode e)
              where
                fns = filter isInternal . s_toList . view anFree $ an   || names of internal free var references

        || DefPats are thunks which may need to be updated after their evaluation.  The code first updates the thunk to point to
        || the blackhole trampoline code, so that recursive evaluation reports a blackHole immediately.  Then, an update continuation
        || is pushed, so that when the evaluation returns, it first returns to the generic update code (Caddr 1) which performs
        || the update, capturing the returned value and changing the code pointer to point to the standard unpack code (Caddr 0)
        || An update analysis pass could provide more info on whether an update is required for a thunk or not, but a simple
        || optimization here is that no update is required for any pat returning a constructor, since it is already WHNF
        buildCodeDef (DefPat p e an)
            = mkCode <$> inDefEnv loc [] fns (buildCode e)
              where
                fns        = filter isInternal . s_toList . view anFree $ an    || names of free var references
                bhInst     = Update r2Opr (Imm (Caddr blackHolePtr))            || update thunk to blackHole trampoline during evaluation
                contInst   = PushCont updatePtr [r2Opr]                         || push a continuation to update thunk with result
                noUpdate   = isCtorCall e || DEBUG \/ isNormThunk p
                mkCode eis = eis,                     if noUpdate
                mkCode eis = bhInst : contInst : eis, otherwise

                || simple stand-in for no-update check on a DefPat -- should be expanded with demand/escape
                || analysis, but for now, just don't perform updates on thunks that are simple constructor
                || calls since constructors are WHNF already and don't evaluate their args
                isCtorCall (Ecall (Evar n) args) = isConstructor n
                isCtorCall e                     = False

                || the idea here is that thunks created during normalization aren't explicitly
                || shared, but that's not correct, since they are args to an Ecall that could
                || escape and share the arg.  So this check is disabled, above, but left here
                || to see if it can be further refined
                || isNormThunk (Pvar n) = isInternal n & "`thunk" $isPrefixOf getName n
                || isNormThunk p        = False

        buildCodeDef _ = st_pure []     || no other Defn types should exist, here, but added to remove compiler warning


|| convert one or a group of mutually-recursive top-level defns to their STG equivalent
|| and install it in the code/heap and environment
buildGroup :: globalEnv -> [name] -> state bdSt [value]
buildGroup genv ns
      || defs converted in parallel in two phases: allocate & bind, then create and update,
      || so that recursive references can lookup their values in the bdLenv
    = st_mapM allocObj ns >>= st_mapM createCode
      where

        || allocate a codePtr and heapPtr, create a heapObj and insert it in the heap and environment
        || Top-level functions have a heapPtr address, even though they don't have any free variables to
        || close over, because they may be passed as higher-order functions to other functions.  Any direct
        || use will be optimized to a direct call, though.
        allocObj n
            = st_bind2 allocCode (allocHeap nslots) (mkObj d) >>= insObj
              where
                d      = makeBuiltinCtorDef n,         if isBuiltin n
                       = fromJust (lookupDefn genv n), otherwise
                nslots = 2, if isDefFn d
                       = 3, otherwise   || allocate an extra slot for use by Update

                mkObj d cp hp
                    = st_pure (n, cp, hp, d) << updateHeap hp (mkInfo d, [])
                      where
                        mkInfo (DefFn n args e an) = Hfun cp (fromJust (lookupArity genv n))    || not #args, because ctors have empty arg fields
                        mkInfo (DefPat p e an)     = Hthunk cp
                        mkInfo d                   = error "buildGroup: bad defn in mkObj"

                insObj (n, cp, hp, d)
                    = st_pure (cp, v, d) << bd_over bdLenv (m_insert cmpname n (Imm v))
                      where
                        v = Haddr hp

        || create the code (stgInsns) for the defn and update the object's codePtr
        createCode (cp, v, d)
            = st_bind2 (buildCode d) (bd_view bdLoc) updateObj
              where
                updateObj ins loc
                    = st_pure v << updateCode cp (loc', ins)
                      where
                        loc' = mergeLocInfo (defnLocInfo d) loc

|| collect all top-level defs and their top-level dependencies reachable from a starting list of names
|| also include stdlib.blackHole, since DefPat thunks implicitly use them during evaluation.
reachableFrom :: [name] -> globalEnv -> m_map name [name]
reachableFrom ns genv
    = walk m_empty (stdBlackHoleName : ns)
      where
        walk seen [] = seen
        walk seen (n : ns)
            = walk seen ns,             if m_member cmpname n seen
            = walk seen' (ns ++ deps'), if isBuiltin n \/ isJust md
            = errNotFound n,            otherwise
              where
                md            = lookupDefn genv n
                d             = fromJust md
                deps          = filter topLevel . s_toList . view defnAnFree $ d        || find all top-level and builtin constructor dependencies
                deps'         = [],   if isBuiltin n                                    || no deps for builtin constructors
                              = deps, otherwise
                seen'         = m_insert cmpname n deps' seen
                errNotFound n = error ("buildStg: " ++ showName n ++ " not found")

                topLevel n = isConstructor n \/ ~isInternal n & ~isBuiltin n

|| make a top-level start function which evaluates the entry function of type IO ()
|| and then exits
makeStart :: name -> globalEnv -> (name, globalEnv)
makeStart n genv
    = errNoSpec,      if isNothing mspec
    = errNotIO,       if ~iOUnitSpec spec
    = (start, genv'), otherwise
      where
        mspec      = lookupSpec genv n
        spec       = (view defnType . fromJust) mspec
        errNoSpec  = error ("cannot find spec for " ++ showName n)
        errNotIO   = error ("entry function must be type 'io ()', but is type '" ++ showAst spec ++ "'")
        start      = Qualified (getModName n) "!start"
        fvs        = s_fromList cmpname [n, builtinUnitName, builtinExitName]
        anno       = set anFree fvs (genvAnno genv)
        startFn    = Ecase Csingle (Ecall (Evar n) [builtinUnit]) [CaseAlt Pwildcard (Ecall builtinExit [EprimInt 0])]
        def        = DefPat (Pvar start) startFn anno
        genv'      = over (composeLens genvMod modDefns) (insDefnMapUnchecked def) genv

        || must be "io ()" or its equivalent type expansion "() -> ((), ())"        
        iOUnitSpec t
            = _eq cmptexpr t (Tarr builtinTypeUnit (Ttuple [builtinTypeUnit, builtinTypeUnit])) \/
              _eq cmptexpr t (Tname (Qualified "io" "io") [builtinTypeUnit])

|| build the STG code & heap reachable from the start name, and return the top-level
|| localEnv map, along with the stg code and heap
buildStg :: name -> globalEnv -> (localEnv, stgCode, stgHeap)
buildStg n genv
    = (view bdLenv st', view bdCode st', view bdHeap st')
      where
        (start', genv') = makeStart n genv
        topLevel        = reachableFrom [start'] genv'
        sccs            = findSCCs cmpname . makeDependencyGraph cmpname . m_toList $ topLevel
        (_, st)         = st_mapM_ (buildGroup genv') sccs (initBdSt genv')
        Imm (Haddr bhp) = fromJust . m_lookup cmpname stdBlackHoleName . view bdLenv $ st
        bhTramp         = fromJust . m_lookup cmpcodePtr blackHolePtr . view bdCodeMap $ st     || get existing blackHole trampoline code obj
        bhTramp'        = setSnd bhTramp [Enter bhp []]                                         || update trampoline to enter actual blackHole closure
        st'             = over bdCodeMap (m_insert cmpcodePtr blackHolePtr bhTramp') st         || replace with new trampoline code
