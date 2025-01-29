|| serialize.m -- conversions between data structures and serialized character streams


%export +

%import <bag>
%import <either>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <lens>
%import <set>
%import <state>                 (>>=)/st_bind (<$>)/st_fmap (<*>)/st_apply
%import "exception"
%import "grammar"
%import "module"
%import "name"


|| serializer and deserializer types
serializer *   == * -> showS
deserializer * == state string *


|| basic type serialization

|| used to encode positive numbers < 96, e.g. ctor encodings
serialCtor :: serializer int
serialCtor n = (decode (n + 32) :) 

deserialCtor :: deserializer int
deserialCtor []       = error "deserialCtor: []"
deserialCtor (c : cs) = (code c - 32, cs)

serialChar :: serializer char
serialChar c = (c :)

deserialChar :: deserializer char
deserialChar []       = error "deserialChar: []"
deserialChar (c : cs) = (c, cs)

|| serialize a positive integer
serialPosInt :: serializer int
serialPosInt n
    = go n 0 ""
      where
        go 0 sz dig = serialCtor sz . (dig ++)
        go n sz dig = go ((div) n 10) (sz + 1) (decode ((mod) n 10 + code '0') : dig)

deserialPosInt :: deserializer int
deserialPosInt
    = deserialCtor >>= splitAt >>= (st_pure . intval)

|| serialize a signed integer
serialInt :: serializer int
serialInt n
    = serialCtor 1 . serialPosInt (neg n), if n < 0
    = serialCtor 0 . serialPosInt n,       otherwise

deserialInt :: deserializer int
deserialInt
    = st_liftA2 mkInt deserialCtor deserialPosInt
      where
        mkInt 0 n = n
        mkInt 1 n = neg n
        mkInt _ _ = undef       || added to remove compiler warning

serialString :: serializer string
serialString s = serialPosInt (#s) . (s ++)

deserialString :: deserializer string
deserialString
    = deserialPosInt >>= splitAt

serialBool :: serializer bool
serialBool False = serialCtor 0
serialBool True  = serialCtor 1

deserialBool :: deserializer bool
deserialBool
    = encBool <$> deserialCtor
      where
        encBool 0 = False
        encBool 1 = True
        encBool _ = undef       || added to remove compiler warning

serialTup2 :: serializer * -> serializer ** -> serializer (*, **)
serialTup2 sa sb (a, b) = sa a . sb b

deserialTup2 :: deserializer * -> deserializer ** -> deserializer (*, **)
deserialTup2 = st_liftA2 pair

serialTup3 :: serializer * -> serializer ** -> serializer *** -> serializer (*, **, ***)
serialTup3 sa sb sc (a, b, c) = sa a . sb b . sc c

deserialTup3 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer (*, **, ***)
deserialTup3 = st_liftA3 triple

serialTup4 :: serializer * -> serializer ** -> serializer *** -> serializer **** -> serializer (*, **, ***, ****)
serialTup4 sa sb sc sd (a, b, c, d) = sa a . sb b . sc c . sd d

deserialTup4 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer (*, **, ***, ****)
deserialTup4 = st_liftA4 mkTup4 where mkTup4 a b c d = (a, b, c, d)

serialTup5 :: serializer * -> serializer ** -> serializer *** -> serializer **** -> serializer ***** -> serializer (*, **, ***, ****, *****)
serialTup5 sa sb sc sd se (a, b, c, d, e) = sa a . sb b . sc c . sd d . se e

deserialTup5 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer ***** -> deserializer (*, **, ***, ****, *****)
deserialTup5 = st_liftA5 mkTup5 where mkTup5 a b c d e = (a, b, c, d, e)

serialTup6 :: serializer * -> serializer ** -> serializer *** -> serializer **** -> serializer ***** -> serializer ****** ->
              serializer (*, **, ***, ****, *****, ******)
serialTup6 sa sb sc sd se sf (a, b, c, d, e, f) = sa a . sb b . sc c . sd d . se e . sf f

deserialTup6 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer ***** -> deserializer ****** ->
                deserializer (*, **, ***, ****, *****, ******)
deserialTup6 = st_liftA6 mkTup6 where mkTup6 a b c d e f = (a, b, c, d, e, f)

serialList :: serializer * -> serializer [*]
serialList sx xs = serialPosInt (#xs) . converse (foldr sx) xs

deserialList :: deserializer * -> deserializer [*]
deserialList ds
    = deserialPosInt >>= d_count
      where
        d_count n = st_pure [],                         if n <= 0
                  = st_liftA2 (:) ds (d_count (n - 1)), otherwise

serialSet :: serializer * -> serializer (s_set *)
serialSet ss = serialList ss . s_toList

deserialSet :: ordI * -> deserializer * -> deserializer (s_set *)
deserialSet cmp ds = deserialList ds >>= (st_pure . s_fromList cmp)

serialBag :: serializer * -> serializer (b_bag *)
serialBag ss = serialList (serialTup2 ss serialPosInt) . b_toList

deserialBag :: ordI * -> deserializer * -> deserializer (b_bag *)
deserialBag cmp ds = deserialList (deserialTup2 ds deserialPosInt) >>= (st_pure . b_fromCountList cmp)

serialEither :: serializer * -> serializer ** -> serializer (either * **)
serialEither sl sr (Left x)  = serialCtor 0 . sl x
serialEither sl sr (Right x) = serialCtor 1 . sr x

deserialEither :: deserializer * -> deserializer ** -> deserializer (either * **)
deserialEither dl dr
    = deserialCtor >>= dse
      where
        dse 0 = Left  <$> dl
        dse 1 = Right <$> dr
        dse _ = undef       || added to remove compiler warning


|| AST serialization

serialCaseSel :: serializer caseSel
serialCaseSel Cnone     = serialCtor 0
serialCaseSel Csingle   = serialCtor 1
serialCaseSel Cpriority = serialCtor 2
serialCaseSel Cvectored = serialCtor 3

deserialCaseSel :: deserializer caseSel
deserialCaseSel 
    = ctor <$> deserialCtor
      where
        ctor 0 = Cnone
        ctor 1 = Csingle
        ctor 2 = Cpriority
        ctor 3 = Cvectored
        ctor _ = undef  || added to remove compiler warning

serialMultSel :: serializer multSel
serialMultSel Mfun = serialCtor 0
serialMultSel Mpat = serialCtor 1

deserialMultSel :: deserializer multSel
deserialMultSel 
    = ctor <$> deserialCtor
      where
        ctor 0 = Mfun
        ctor 1 = Mpat
        ctor _ = undef  || added to remove compiler warning


serialLoc :: serializer location
serialLoc = serialTup3 serialString serialPosInt serialPosInt

deserialLoc :: deserializer location
deserialLoc = deserialTup3 deserialString deserialPosInt deserialPosInt

serialAnno :: serializer anno
serialAnno = serialTup6 serialLoc serialBool serialPosInt serialPosInt (serialSet serialName) (serialSet serialCtor)

deserialAnno :: deserializer anno
deserialAnno = deserialTup6 deserialLoc deserialBool deserialPosInt deserialPosInt (deserialSet cmpname deserialName) (deserialSet cmpint deserialCtor)

serialName :: serializer name
serialName (Unqualified n)  = serialCtor 0 . serialString n
serialName (Unresolved m n) = serialCtor 1 . serialString m . serialString n
serialName (Qualified m n)  = serialCtor 2 . serialString m . serialString n
serialName (Private m n)    = serialCtor 3 . serialString m . serialString n
serialName (Builtin n)      = serialCtor 4 . serialString n
serialName (Internal n d)   = serialCtor 5 . serialString n  . serialPosInt d

deserialName :: deserializer name
deserialName
    = deserialCtor >>= ctor
      where
        ctor 0 =           Unqualified <$> deserialString
        ctor 1 = st_liftA2 Unresolved  deserialString deserialString
        ctor 2 = st_liftA2 Qualified   deserialString deserialString
        ctor 3 = st_liftA2 Private     deserialString deserialString
        ctor 4 =           Builtin <$> deserialString
        ctor 5 = st_liftA2 Internal    deserialString deserialPosInt
        ctor _ = undef  || added to remove compiler warning

serialDefnMap :: serializer defnMap
serialDefnMap = serialList (serialTup2 serialName serialAst) . m_toList

deserialDefnMap :: deserializer defnMap
deserialDefnMap = m_fromList cmpname <$> deserialList (deserialTup2 deserialName deserialAst)

serialNameMap :: serializer nameMap
serialNameMap = serialList (serialTup2 serialName serialName) . m_toList

deserialNameMap :: deserializer nameMap
deserialNameMap = m_fromList cmpname <$> deserialList (deserialTup2 deserialName deserialName)

serialAst :: serializer ast
serialAst (Evar n)            = serialCtor  0 . serialName n
serialAst (Eint n)            = serialCtor  1 . serialInt n
serialAst (Echar c)           = serialCtor  2 . serialChar c
serialAst (Estring s)         = serialCtor  3 . serialString s
serialAst (Eap e1 e2)         = serialCtor  4 . serialAst e1 . serialAst e2
serialAst (Ecall e args)      = serialCtor  5 . serialAst e . serialList serialAst args
serialAst (Econd conds)       = serialCtor  6 . serialList serialAst conds
serialAst (Ecase s e alts)    = serialCtor  7 . serialCaseSel s . serialAst e . serialList serialAst alts
serialAst (Efail e)           = serialCtor  8 . serialAst e
serialAst (Efatbar e1 e2 e3)  = serialCtor  9 . serialAst e1 . serialAst e2 . serialAst e3
serialAst (Elet r ds ps e)    = serialCtor 10 . serialBool r . serialDefnMap ds . serialList serialAst ps . serialAst e
serialAst (ElistComp e qs an) = serialCtor 11 . serialAst e . serialList serialAst qs . serialAnno an
serialAst (Cif e1 e2)         = serialCtor 12 . serialAst e1 . serialAst e2
serialAst (Cdefault e)        = serialCtor 13 . serialAst e
serialAst (CaseAlt p e)       = serialCtor 14 . serialAst p . serialAst e
serialAst (Qgen ps e)         = serialCtor 15 . serialList serialAst ps . serialAst e
serialAst (Qrecur p e1 e2)    = serialCtor 16 . serialAst p . serialAst e1 . serialAst e2
serialAst (Qguard e)          = serialCtor 17 . serialAst e
serialAst (Pvar n)            = serialCtor 18 . serialName n
serialAst (Plit e)            = serialCtor 19 . serialAst e
serialAst (Pctor n ps)        = serialCtor 20 . serialName n . serialList serialAst ps
serialAst Pwildcard           = serialCtor 21
serialAst (Tname n tes)       = serialCtor 22 . serialName n . serialList serialAst tes
serialAst (Tvar n)            = serialCtor 23 . serialPosInt  n
serialAst (Ttuple tes)        = serialCtor 24 . serialList serialAst tes
serialAst (Tlist te)          = serialCtor 25 . serialAst te
serialAst (Tarr t1 t2)        = serialCtor 26 . serialAst t1 . serialAst t2
serialAst (Tstrict te)        = serialCtor 27 . serialAst te
serialAst (DefFn n ps e an)   = serialCtor 28 . serialName n . serialList serialAst ps . serialAst e . serialAnno an
serialAst (DefMult m ds)      = serialCtor 29 . serialMultSel m . serialList serialAst ds
serialAst (DefPat p e an)     = serialCtor 30 . serialAst p . serialAst e . serialAnno an
serialAst (DefSyn tf te an)   = serialCtor 31 . serialAst tf . serialAst te . serialAnno an
serialAst (DefData tf tfs an) = serialCtor 32 . serialAst tf . serialList serialAst tfs . serialAnno an
serialAst (DefSpec tf te an)  = serialCtor 33 . serialAst tf . serialAst te . serialAnno an
serialAst (DefAbs tf ds t an) = serialCtor 34 . serialAst tf . serialList serialAst ds . serialAst t . serialAnno an
serialAst (EprimInt n)        = serialCtor 35 . serialInt n
serialAst (EprimChar c)       = serialCtor 36 . serialChar c
serialAst (EprimString s)     = serialCtor 37 . serialString s

deserialAst :: deserializer ast
deserialAst
    = deserialCtor >>= ctor
      where
        deserialAstList = deserialList deserialAst

        ctor  0 =           Evar        <$> deserialName
        ctor  1 =           Eint        <$> deserialInt
        ctor  2 =           Echar       <$> deserialChar
        ctor  3 =           Estring     <$> deserialString
        ctor  4 = st_liftA2 Eap             deserialAst         deserialAst
        ctor  5 = st_liftA2 Ecall           deserialAst         deserialAstList
        ctor  6 =           Econd       <$> deserialAstList
        ctor  7 = st_liftA3 Ecase           deserialCaseSel     deserialAst        deserialAstList
        ctor  8 =           Efail       <$> deserialAst
        ctor  9 = st_liftA3 Efatbar         deserialAst         deserialAst        deserialAst
        ctor 10 = st_liftA4 Elet            deserialBool        deserialDefnMap    deserialAstList    deserialAst
        ctor 11 = st_liftA3 ElistComp       deserialAst         deserialAstList    deserialAnno
        ctor 12 = st_liftA2 Cif             deserialAst         deserialAst
        ctor 13 =           Cdefault    <$> deserialAst
        ctor 14 = st_liftA2 CaseAlt         deserialAst         deserialAst
        ctor 15 = st_liftA2 Qgen            deserialAstList     deserialAst
        ctor 16 = st_liftA3 Qrecur          deserialAst         deserialAst        deserialAst
        ctor 17 =           Qguard      <$> deserialAst
        ctor 18 =           Pvar        <$> deserialName
        ctor 19 =           Plit        <$> deserialAst
        ctor 20 = st_liftA2 Pctor           deserialName        deserialAstList
        ctor 21 = st_pure   Pwildcard
        ctor 22 = st_liftA2 Tname           deserialName        deserialAstList
        ctor 23 =           Tvar        <$> deserialPosInt
        ctor 24 =           Ttuple      <$> deserialAstList
        ctor 25 =           Tlist       <$> deserialAst
        ctor 26 = st_liftA2 Tarr            deserialAst         deserialAst
        ctor 27 =           Tstrict     <$> deserialAst
        ctor 28 = st_liftA4 DefFn           deserialName        deserialAstList    deserialAst       deserialAnno
        ctor 29 = st_liftA2 DefMult         deserialMultSel     deserialAstList
        ctor 30 = st_liftA3 DefPat          deserialAst         deserialAst        deserialAnno
        ctor 31 = st_liftA3 DefSyn          deserialAst         deserialAst        deserialAnno
        ctor 32 = st_liftA3 DefData         deserialAst         deserialAstList    deserialAnno
        ctor 33 = st_liftA3 DefSpec         deserialAst         deserialAst        deserialAnno
        ctor 34 = st_liftA4 DefAbs          deserialAst         deserialAstList    deserialAst       deserialAnno
        ctor 35 =           EprimInt    <$> deserialInt
        ctor 36 =           EprimChar   <$> deserialChar
        ctor 37 =           EprimString <$> deserialString
        ctor n  = error ("deserialCtor: unknown ctor id " ++ showint n)

|| module serialization

serialFileSpec :: serializer fileSpec
serialFileSpec (FileSpec ps n) = serialList serialString ps . serialString n

deserialFileSpec :: deserializer fileSpec
deserialFileSpec = st_liftA2 FileSpec (deserialList deserialString) deserialString

serialLibQual :: serializer libQual
serialLibQual LibUnqualified = serialCtor 0
serialLibQual LibQualified   = serialCtor 1

deserialLibQual :: deserializer libQual
deserialLibQual
    = deserialCtor >>= ctor
      where
        ctor 0 = st_pure LibUnqualified
        ctor 1 = st_pure LibQualified
        ctor _ = undef  || added to remove compiler warning

serialLibAs :: serializer libAs
serialLibAs LibNoAs   = serialCtor 0
serialLibAs (LibAs s) = serialCtor 1 . serialString s

deserialLibAs :: deserializer libAs
deserialLibAs
    = deserialCtor >>= ctor
      where
        ctor 0 = st_pure LibNoAs
        ctor 1 = LibAs <$> deserialString
        ctor _ = undef  || added to remove compiler warning

serialLibPart :: serializer libPart
serialLibPart (LibIdent n l)  = serialCtor 0 . serialName n     . serialLoc l
serialLibPart (LibRemove n l) = serialCtor 1 . serialName n     . serialLoc l
serialLibPart (LibFile f l)   = serialCtor 2 . serialFileSpec f . serialLoc l
serialLibPart (LibAll l)      = serialCtor 3 . serialLoc l              

deserialLibPart :: deserializer libPart
deserialLibPart
    = deserialCtor >>= ctor
      where
        ctor 0 = st_liftA2 LibIdent     deserialName     deserialLoc
        ctor 1 = st_liftA2 LibRemove    deserialName     deserialLoc
        ctor 2 = st_liftA2 LibFile      deserialFileSpec deserialLoc
        ctor 3 =           LibAll <$>   deserialLoc
        ctor _ = undef  || added to remove compiler warning

serialLibAlias :: serializer libAlias
serialLibAlias (Alias n1 n2)   = serialCtor 0 . serialName n1 . serialName n2
serialLibAlias (AliasRemove n) = serialCtor 1 . serialName n

deserialLibAlias :: deserializer libAlias
deserialLibAlias
    = deserialCtor >>= dla
      where
        dla 0 = st_liftA2 Alias deserialName deserialName
        dla 1 = AliasRemove <$> deserialName
        dla _ = undef       || added to remove compiler warning

serialLibBinding :: serializer libBinding
serialLibBinding (BindVar n e)   = serialCtor 0 . serialName n . serialAst e
serialLibBinding (BindType n te) = serialCtor 1 . serialName n . serialAst te

deserialLibBinding :: deserializer libBinding
deserialLibBinding
    = deserialCtor >>= dlb
      where
        dlb 0 = st_liftA2 BindVar  deserialName deserialAst
        dlb 1 = st_liftA2 BindType deserialName deserialAst
        dlb _ = undef       || added to remove compiler warning

serialLibEnv :: serializer libEnv
serialLibEnv = serialTup6 serialFileSpec serialLoc serialLibQual serialLibAs (serialList serialLibBinding) (serialList serialLibAlias)

deserialLibEnv :: deserializer libEnv
deserialLibEnv = deserialTup6 deserialFileSpec deserialLoc deserialLibQual deserialLibAs (deserialList deserialLibBinding) (deserialList deserialLibAlias)

serialBuildStatus :: serializer buildStatus
serialBuildStatus Unbuilt    = serialCtor 0
serialBuildStatus Built      = serialCtor 1
serialBuildStatus Serialized = serialCtor 2

deserialBuildStatus :: deserializer buildStatus
deserialBuildStatus
    = mkbs <$> deserialCtor
      where
        mkbs 0 = Unbuilt
        mkbs 1 = Built
        mkbs 2 = Serialized
        mkbs _ = undef  || added to remove compiler warning

serialModule :: serializer module
serialModule (Module fs bs exs ins defs pats specs env)
    = serialFileSpec fs                                         .
      serialBuildStatus Serialized                              .
      serialEither (serialList serialLibPart) serialNameMap exs .
      serialList serialLibEnv ins                               .
      serialDefnMap defs                                        .
      serialList serialAst pats                                 .
      serialDefnMap specs                                       .
      serialNameMap env

deserialModule :: deserializer module
deserialModule = ((st_liftA6 Module
                     deserialFileSpec
                     deserialBuildStatus
                     (deserialEither (deserialList deserialLibPart) deserialNameMap)
                     (deserialList deserialLibEnv)
                     deserialDefnMap
                     (deserialList deserialAst))  <*>
                     deserialDefnMap)             <*>
                     deserialNameMap
