|| predef.m -- references to pre-defined types, variables, and names that are builtin or in stdlib


%export + -p_spec

%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "config"
%import "grammar"
%import "name"
%import "parser"
%import "tokenizer"


stdlibPath :: [string]
stdlibPath =  split (hd filePathSeparator) miranda2LibPath

makeBuiltinName :: string -> name
makeBuiltinName s = Builtin s

makeBuiltinType :: string -> texpr
makeBuiltinType s = Tname (makeBuiltinName s) []

makeBuiltinVar :: string -> expr
makeBuiltinVar s = Evar (makeBuiltinName s)

makeStdlibName :: string -> name
makeStdlibName s = Qualified stdlibModuleName s

makeStdlibType :: string -> texpr
makeStdlibType s = Tname (makeStdlibName s) []

makeStdlibVar :: string -> expr
makeStdlibVar s = Evar (makeStdlibName s)

|| make a Miranda2 list expression from a list of expressions
makeList :: [expr] -> expr
makeList elts = foldr (ap2 builtinCons) builtinNil elts

|| make a Miranda2 tuple expression from a list of expressions
makeTupleExpr :: [expr] -> expr
makeTupleExpr [elt] = elt       || single element
makeTupleExpr elts
    = foldl Eap ntuple elts     || tuple of elements
      where
        ntuple = Evar (builtinTupleName (#elts))

|| make a Miranda2 tuple pattern from a list of names
makeTuplePat :: [name] -> pat
makeTuplePat [n] = Pvar n               || single name
makeTuplePat ns  = Pctor (builtinTupleName (#ns)) (map Pvar ns)

|| make a generic Tuple constructor type from its constructor name
|| e.g. Tuple3 ==> * -> ** -> *** -> Ttuple * ** ***
makeTupleCtorType :: name -> texpr
makeTupleCtorType n
    = error ((shows "makeTupleCtorType: " . showsName n) " is not a Tuple constructor"), if isNothing msize
    = foldr Tarr (Ttuple tvars) tvars,                                                   otherwise
      where
        msize = stripPrefix cmpchar "Tuple" (getName n)
        tsize = intval (fromJust msize)
        tvars = map Tvar [1 .. tsize]

|| make a generic Tuple type from its constructor name
|| e.g. Tuple3 ==> Ttuple * ** ***
makeTupleType :: name -> texpr
makeTupleType = baseType . makeTupleCtorType

|| make a function name for a tuple, given the base function name and the tuple arity
|| note that this returns an Unqualified name, rather than Builtin or stdlib, because cmptupleN and showtupleN
|| functions can be added as needed in other modules (e.g. mirandaExtensions)
makeTupleFnName :: string -> int -> name
makeTupleFnName s n = Unqualified (s ++ "tuple" ++ showint n)

|| make Tuple comparison function and show function vars from their types
makeTupleCmp, makeTupleShow :: int -> expr
makeTupleCmp  = Evar . makeTupleFnName "cmp"
makeTupleShow = Evar . makeTupleFnName "show"

|| generate an expression from a location
genLoc :: location -> expr
genLoc (mn, il, ic) = makeTupleExpr [Estring mn, EprimInt il, EprimInt ic]


|| make a builtin constructor definition based upon the name
makeBuiltinCtorDef :: name -> defn
makeBuiltinCtorDef n
    = DefFn n [] (builtinPack n tag aty) an
      where
        an   = locAnno (initLocation "builtin")

        aty  = 0,                   if _eq cmpname n builtinNilName \/ _eq cmpname n builtinUnitName
             = 2,                   if _eq cmpname n builtinConsName
             = builtinTupleArity n, if isBuiltinTuple n
             = err,                 otherwise

        tag  = 1,                   if _eq cmpname n builtinConsName
             = 0,                   otherwise

        err  = error ("makeBuiltinCtorDef: undefined name " ++ showName n)

|| stdlib names used in compiler
stdFalseName     = makeStdlibName "False"
stdTrueName      = makeStdlibName "True"
stdAndName       = makeStdlibName "&"
stdNegName       = makeStdlibName "neg"
stdIntCtorName   = makeStdlibName "I#"
stdCharCtorName  = makeStdlibName "C#"
stdCaseFailName  = makeStdlibName "caseFail"
stdMatchFailName = makeStdlibName "matchFail"
stdBlackHoleName = makeStdlibName "blackHole"

|| stdlib types used in compiler
stdInt      = makeStdlibType "int"
stdNum      = makeStdlibType "num"
stdChar     = makeStdlibType "char"
stdBool     = makeStdlibType "bool"
stdString   = makeStdlibType "string"
stdOrdering = makeStdlibType "ordering"

|| stdlib vars used in compiler
stdIntCtor          = makeStdlibVar "I#"
stdCharCtor         = makeStdlibVar "C#"
stdEQ               = makeStdlibVar "EQ"
stdLT               = makeStdlibVar "LT"
stdGT               = makeStdlibVar "GT"
stdCmpInt           = makeStdlibVar "cmpint"
stdCmpChar          = makeStdlibVar "cmpchar"
stdCmpWord          = makeStdlibVar "cmpword#"
stdCmpList          = makeStdlibVar "cmplist"
stdCmpFn            = makeStdlibVar "cmpFn"
stdCmpTags          = makeStdlibVar "cmpTags"
stdThenCmp          = makeStdlibVar "thenCmp"
stdShowFn           = makeStdlibVar "showFn"
stdShowList         = makeStdlibVar "showlist"
stdCaseFail         = makeStdlibVar "caseFail"
stdMatchFail        = makeStdlibVar "matchFail"
stdError            = makeStdlibVar "error"
stdBlackHole        = makeStdlibVar "blackHole"
stdApply            = makeStdlibVar "$"
stdRapply           = makeStdlibVar "|>"
stdCompose          = makeStdlibVar "."
stdRcompose         = makeStdlibVar ".>"
stdAppend           = makeStdlibVar "++"
stdConverse         = makeStdlibVar "converse"
stdRange            = makeStdlibVar "range"
stdRangeFrom        = makeStdlibVar "rangeFrom"
stdRangeBy          = makeStdlibVar "rangeBy"
stdRangeByFrom      = makeStdlibVar "rangeByFrom"
stdReadByteStream   = makeStdlibVar "readByteStream"
stdSub              = makeStdlibVar "-"

|| stdlib equality comparisons used in compiler
stdEqInt  = makeStdlibVar "=="
stdEqChar = makeStdlibVar "==."
stdEq     = makeStdlibVar "_eq"

eqOpForLit (Eint n)      = stdEqInt
eqOpForLit (Echar c)     = stdEqChar
eqOpForLit (Estring s)   = Eap stdEq (Eap stdCmpList stdCmpChar)
eqOpForLit (EprimInt n)  = Eap stdEq stdCmpWord
eqOpForLit (EprimChar c) = Eap stdEq stdCmpWord
eqOpForLit w             = stdEqInt

|| builtin simple types
builtinTypeType  = makeBuiltinType "type"
builtinTypeWord  = makeBuiltinType "word#"
builtinTypeUnit  = makeBuiltinType "unit"       || use "unit" instead of "()" to disambiguate with unit constructor

|| builtin names used in compiler
builtinPackName    = makeBuiltinName "pack"
builtinSelName     = makeBuiltinName "sel"
builtinNilName     = makeBuiltinName "Nil"      || use "Nil" instead of "[]" because it simplifies the test for a constructor name
builtinConsName    = makeBuiltinName ":"        || begins with ":", so it matches the constructor name test
builtinUnitName    = makeBuiltinName "Unit"     || use "Unit" instead of "()" because it simplifies the test for a constructor name
builtinGetTagName  = makeBuiltinName "getTag#"
builtinExitName    = makeBuiltinName "exit#"

builtinTupleName :: int -> name
builtinTupleName n = makeBuiltinName ((shows "Tuple" . showsint n) "")

builtinTupleArity :: name -> int
builtinTupleArity (Builtin n) = intval ns where Just ns = stripPrefix cmpchar "Tuple" n
builtinTupleArity _           = error "builtinTupleArity: non-tuple name"

|| builtin pseudo-functions
|| these functions are polymorphic, so to be able to typecheck them, the constructor name
|| is passed in and the functions are a texpr (Tname) instead of an expr (Evar) to tag them as such
builtinPack :: name -> int -> int -> expr
builtinPack n t na = Tname builtinPackName [Tname n [], EprimInt t, EprimInt na]        || ctor name, ctor tag, #args

builtinSel :: name -> int -> expr
builtinSel  n i    = Tname builtinSelName  [Tname n [], EprimInt i]                     || ctor name, ctor field idx

|| named Fail and Fatbar pseudo-functions; name will be uniquified lexically during rewrite
|| to tie Fails to their corresponding Fatbar and tracked through free vars analysis
|| to allow the Fail to reference the dynamically-generated thunk (and stack) for the alt leg
builtinFail :: expr
builtinFail   = Efail . Evar . Unqualified $ "`fail"

builtinFatbar :: expr -> expr -> expr
builtinFatbar = Efatbar . Pvar . Unqualified $ "`fail"

|| builtin functions
builtinCmp              = makeBuiltinVar "cmp#"
builtinAdd              = makeBuiltinVar "+#"
builtinSub              = makeBuiltinVar "-#"
builtinMul              = makeBuiltinVar "*#"
builtinDiv              = makeBuiltinVar "div#"
builtinMod              = makeBuiltinVar "mod#"
builtinAnd              = makeBuiltinVar "band#"
builtinOr               = makeBuiltinVar "bor#"
builtinXor              = makeBuiltinVar "bxor#"
builtinNot              = makeBuiltinVar "bnot#"
builtinShl              = makeBuiltinVar "bshl#"
builtinShr              = makeBuiltinVar "bshr#"
builtinGetTag           = makeBuiltinVar "getTag#"
builtinExit             = makeBuiltinVar "exit#"
builtinAllocByteStream  = makeBuiltinVar "allocByteStream#"
builtinAllocFileStream  = makeBuiltinVar "allocFileStream#"
builtinReadByteStream   = makeBuiltinVar "readByteStream#"
builtinWriteByteStream  = makeBuiltinVar "writeByteStream#"
builtinAllocArray       = makeBuiltinVar "allocArray#"
builtinFillArray        = makeBuiltinVar "fillArray#"
builtinCopyArray        = makeBuiltinVar "copyArray#"
builtinReadArray        = makeBuiltinVar "readArray#"
builtinWriteArray       = makeBuiltinVar "writeArray#"
builtinOpenFileRead     = makeBuiltinVar "openFileRead#"
builtinOpenFileWrite    = makeBuiltinVar "openFileWrite#"
builtinOpenFileAppend   = makeBuiltinVar "openFileAppend#"
builtinCloseFile        = makeBuiltinVar "closeFile#"
builtinReadFile         = makeBuiltinVar "readFile#"
builtinWriteFile        = makeBuiltinVar "writeFile#"
builtinMtimeFile        = makeBuiltinVar "mtimeFile#"
builtinGetArg           = makeBuiltinVar "getArg#"
builtinSystemCmd        = makeBuiltinVar "systemCmd#"
builtinUnit             = makeBuiltinVar "Unit"
builtinNil              = makeBuiltinVar "Nil"
builtinCons             = makeBuiltinVar ":"

|| builtin environment
builtinEnv :: nameMap
builtinEnv
    = foldl ins m_empty [ builtinTypeType, builtinTypeUnit, builtinTypeWord,
                          builtinUnit, builtinNil, builtinCons,
                          builtinCmp, builtinAdd, builtinSub, builtinMul, builtinDiv, builtinMod,
                          builtinAnd, builtinOr, builtinXor, builtinNot, builtinShl, builtinShr,
                          builtinAllocByteStream, builtinAllocFileStream, builtinReadByteStream, builtinWriteByteStream,
                          builtinAllocArray, builtinFillArray, builtinCopyArray, builtinReadArray, builtinWriteArray,
                          builtinGetTag,
                          builtinExit, builtinOpenFileRead, builtinOpenFileWrite, builtinOpenFileAppend, builtinCloseFile,
                          builtinReadFile, builtinWriteFile, builtinMtimeFile, builtinGetArg, builtinSystemCmd
                        ]
      where
        || insert both qualified and unqualified names
        ins env (Tname n ts) = m_insert cmpname (unqualifyName n) n . m_insert cmpname n n $ env
        ins env (Evar  n)    = m_insert cmpname (unqualifyName n) n . m_insert cmpname n n $ env
        ins env _            = undef     || added to remove compiler warning

builtinSpecs :: defnMap
builtinSpecs
    = foldl insertSpec m_empty . map p_spec $ specs
      where
        specs = [
                || constructors
                  "Unit              :: unit"
                , "Nil               :: [*]"
                , ":                 :: * -> [*] -> [*]"

                || primitive arithmetic operations on word#
                , "cmp#              :: word# -> word# -> word#"        || cmp returns 0# -> EQ, 1# -> LT, 2# -> GT
                , "+#                :: word# -> word# -> word#"
                , "-#                :: word# -> word# -> word#"
                , "*#                :: word# -> word# -> word#"
                , "div#              :: word# -> word# -> word#"
                , "mod#              :: word# -> word# -> word#"

                || primitive bit-wise operations on word#
                , "band#             :: word# -> word# -> word#"
                , "bor#              :: word# -> word# -> word#"
                , "bxor#             :: word# -> word# -> word#"
                , "bnot#             :: word# -> word#"
                , "bshl#             :: word# -> word# -> word#"
                , "bshr#             :: word# -> word# -> word#"

                || operations on packed byte streams
                , "allocByteStream#  :: word# -> word#"                 || return a ref to a newly-allocated byteStream
                , "allocFileStream#  :: word# -> word# -> word#"        || return a ref to a newly-allocated byteStream with specified fd field
                , "readByteStream#   :: word# -> word#"                 || read a byte from byteStream ref
                , "writeByteStream#  :: word# -> word# -> word#"        || write a byte to a byteStream ref, returning success/fail indication

                || operations on arrays
                , "allocArray#       :: word# -> word#"                 || alloc a contiguous array of N entries (uninitialized), and return its ref
                , "fillArray#        :: word# -> * -> unit"             || fill an existing array with a constant value
                , "copyArray#        :: word# -> word# -> unit"         || copy contents of src array to dst array (already allocated, sizes must match!)
                , "readArray#        :: word# -> word# -> *"            || read a value from the array ref at the given index
                , "writeArray#       :: word# -> word# -> * -> unit"    || write a value to the array ref at the given index

                || compiler support
                , "getTag#           :: * -> word#"                     || get tag of a constructor application (for use in ordI instances)

                || system operations
                , "exit#             :: word# -> *"                     || exit with the status word (return type is * for typechecker)
                , "openFileRead#     :: word# -> word#"                 || open file named by a byteStream for read and return a file buffer byteStream
                , "openFileWrite#    :: word# -> word#"                 || open or create file named by a byteStream for write
                , "openFileAppend#   :: word# -> word#"                 || open or create file named by a byteStream for append
                , "closeFile#        :: word# -> unit"                  || close an open file buffer byteStream
                , "readFile#         :: word# -> word#"                 || fill a file buffer byteStream from the file, and return the read status
                , "writeFile#        :: word# -> word#"                 || write a file buffer byteStream to the file, and return the status
                , "mtimeFile#        :: word# -> word#"                 || get the modification timestamp for a file (or 0, if non-existent)
                , "getArg#           :: word# -> word#"                 || get arg count [0] or argument readStream [1 .. argc -1]
                , "systemCmd#        :: word# -> word#"                 || execute the shell command specified by a bytStream and return its int result
                ]
        insertSpec m d = m_insert cmpname (defnName d) d m

|| parse a type specification
|| simple form of the version in mirandaParser to handle just the builtin specs above
p_spec :: string -> defn
p_spec s
    = error ("error parsing '" ++ s ++ "' " ++ view psErrStr ps), if isNothing mspec
    = fromJust mspec,                                             otherwise
      where
        (mspec, ps)                       = parse ("builtin", emptyError, [], tokenize s)
        parse                             = p_liftA2 mkSpec (p_tname $p_left p_colons) p_type
        p_type                            = p_liftA2 mkType p_term (p_optional (p_sym "->" $p_right p_type))
        p_term                            = p_tvar $p_alt p_tname $p_alt p_tlist $p_alt p_ttup
        p_tname                           = p_any $p_bind mkTname
        p_tvar                            = p_any $p_bind mkTvar
        p_tlist                           = Tlist $p_fmap p_inBrackets p_type
        p_ttup                            = Ttuple $p_fmap p_inParens (p_manySepBy p_comma p_type)
        mkTname (Tident n)                = p_pure (Tname (doQual n) [])
        mkTname (Tsymbol n)               = p_pure (Tname (doQual n) [])
        mkTname t                         = p_fail
        mkTvar  (Tsymbol (Unqualified s)) = p_pure . Tvar . length $ s, if all (==. '*') s
                                          = p_fail,                     otherwise
        mkTvar  t                         = p_fail
        mkType t1 mt2                     = t1,                     if isNothing mt2
                                          = Tarr t1 (fromJust mt2), otherwise
        mkSpec n t                        = DefSpec n t emptyAnno

        doQual (Unqualified n)            = Builtin n           || unqualified names in predef are Builtin
        doQual (Unresolved m n)           = Qualified m n       || unresolved qualified names in predef are assumed to be correct
        doQual n                          = n
