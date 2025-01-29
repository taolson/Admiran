|| derive.m -- derive instances of show and ord for user-defined data types and synonyms


%export deriveInstances

%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "exception"
%import "grammar"
%import "module"
%import "name"
%import "predef"


|| derive a show instance spec and defn for a user-defined data type or synonym
deriveShow :: defn -> (defn, defn)
deriveShow d
    = (defSpec, defFn)
      where
        fn          = Unqualified $ "show" ++ getName (defnName d)
        an          = defnAnno d
        tfd         = view defnTform d
        tvs         = tformVars tfd
        instNames   = (zip2 tvs . map (Internal "showtv")) [0 ..]
        spec        = foldr (Tarr . ($Tarr stdString)) (tfd $Tarr stdString) tvs
        go          = Unqualified "go"
        ctors       = view dataCtors d
        goDefs      = map genDef ctors
        goDict      = m_singleton go $ DefMult Mfun goDefs
        e           = Elet False goDict [] (Evar go), if isDefData d
                    = showForType (view defnType d),  otherwise
        defSpec     = DefSpec (Tname fn []) spec an
        defFn       = DefFn fn (map (Pvar . snd) instNames) e an

        showForType (Tvar n)       = Evar . findInstName . Tvar $ n
        showForType (Ttuple ts)    = foldl Eap (makeTupleShow $ #ts) $ map showForType ts
        showForType (Tlist t)      = Eap stdShowList $ showForType t
        showForType (Tarr t1 t2)   = stdShowFn
        showForType (Tstrict t)    = showForType t

        showForType (Tname (Qualified m n) vars)
            = foldl Eap (Evar . Qualified m $ "show" ++ n) $ map showForType vars

        showForType (Tname (Unresolved m n) vars)
            = foldl Eap (Evar . Unresolved m $ "show" ++ n) $ map showForType vars

        showForType (Tname n vars)
            = foldl Eap (Evar . Unqualified $ "show" ++ getName n) $ map showForType vars

        showForType _ = undef      || added to remove compiler warning

        findInstName te            = fromMaybe (Unqualified "undef") . mb_fmap snd . find ((_eq cmptexpr te) . fst) $ instNames

        genDef (Tname n tas)
            = DefFn go [Pctor n (map Pvar vars)] showExpr an
              where
                ns               = Estring $ getName n
                (_, vars)        = genNames "a" 0 tas
                showExpr         = ns,                                                                  if null tas
                                 = ap2 stdAppend (Estring "(") . ap2 stdAppend buildExpr $ Estring ")", otherwise
                buildExpr        = foldr1 appendShow . (ns :) . map mkShow $ zip2 tas [0 ..]
                appendShow s1 s2 = ap2 stdAppend s1 $ ap2 stdAppend (Estring " ") s2
                mkShow (t, i)    = Eap (showForType t) . Evar $ Internal "a" i

         genDef _ = undef       || added to remove compiler warning


|| derive an ord instance spec and defn for a user-defined data type or synonym
deriveOrd :: defn -> (defn, defn)
deriveOrd d
    = (defSpec, defFn)
      where
        fn         = Unqualified $ "cmp" ++ getName (defnName d)
        an         = defnAnno d
        tfd        = view defnTform d
        tvs        = tformVars tfd
        instNames  = zip2 tvs . map (Internal "cmptv") $ [0 ..]
        spec       = foldr (Tarr . cmpSpec) (cmpSpec tfd) tvs
        go         = Unqualified "go"
        ctors      = view dataCtors d
        nullary    = null . tformVars
        vars       = map Unqualified ["a", "b"]
        tagCmp     = DefFn go (map Pvar vars) (Ecall stdCmpTags (map Evar vars)) an
        goDefs     = [genDef a b | a, b <- zip2 ctors [0 ..]],          if #ctors <= 5
                   = [genDef a a | a <- zip2 ctors [0 ..]] ++ [tagCmp], otherwise
        goDict     = m_singleton go $ DefMult Mfun goDefs
        e          = stdCmpTags,                     if isDefData d & all nullary ctors
                   = Elet False goDict [] (Evar go), if isDefData d
                   = cmpForType (view defnType d),   otherwise
        defSpec    = DefSpec (Tname fn []) spec an
        defFn      = DefFn fn (map (Pvar . snd) instNames) e an

        cmpSpec t  = t $Tarr t $Tarr stdOrdering

        cmpForType (Tvar n)       = Evar . findInstName . Tvar $ n
        cmpForType (Ttuple ts)    = foldl Eap (makeTupleCmp $ #ts) $ map cmpForType ts
        cmpForType (Tlist t)      = Eap stdCmpList $ cmpForType t
        cmpForType (Tarr t1 t2)   = stdCmpFn
        cmpForType (Tstrict t)    = cmpForType t

        cmpForType (Tname (Qualified m n) vars)
            = foldl Eap (Evar . Qualified m $ "cmp" ++ n) $ map cmpForType vars

        cmpForType (Tname (Unresolved m n) vars)
            = foldl Eap (Evar . Unresolved m $ "cmp" ++ n) $ map cmpForType vars

        cmpForType (Tname n vars)
            = foldl Eap (Evar . Unqualified $ "cmp" ++ getName n) $ map cmpForType vars

        cmpForType _ = undef    || added to remove compiler warning

        findInstName te           = fromMaybe (Unqualified "undef") . mb_fmap snd . find ((_eq cmptexpr te) . fst) $ instNames

        genDef (c1, tag1) (c2, tag2)
            = DefFn go [genPat c1 "a", genPat c2 "b"] cmpExpr an
              where
                tvs          = tformVars c1
                ctorTvarIdx  = zip2 tvs [0 ..]
                cmpExpr      = stdLT,                                           if tag1 < tag2
                             = stdGT,                                           if tag1 > tag2
                             = stdEQ,                                           if null tvs
                             = foldr1 (ap2 stdThenCmp) (map mkCmp ctorTvarIdx), otherwise
                mkCmp (t, i) = ap2 (cmpForType t) (Evar (Internal "a" i)) (Evar (Internal "b" i))

                genPat (Tname n tas) s
                    = Pctor n (map Pvar vars)
                      where
                        (_, vars) = genNames s 0 tas

                genPat _ _ = undef        || added to remove compiler warning

|| derive any missing instances for ord, show for the dataDefs
deriveInstances :: globalEnv -> excpt globalEnv
deriveInstances genv
    = converse (set genvMod) genv $ex_fmap ex_foldM insInstance m (shows ++ ords)
      where
        m             = view genvMod genv
        defs          = view modDefns m
        pats          = view modPats m
        ddefs         = filter isDerivable . m_elems $ defs
        shows         = map deriveShow . filter (missing "show") $ ddefs
        ords          = map deriveOrd .  filter (missing "cmp")  $ ddefs
        isDerivable d = isDefData d & (not . null . view dataCtors) d \/ isDefSyn d

        insInstance m (ds, df)
            = modInsSpec ds m $ex_bind modInsDef df

        missing s d
            = isNothing (m_lookup cmpname n' defs) & ~any (match n') pats
              where
                n  = defnName d
                n' = Qualified (getModName n) (s ++ getName n)

                match n (DefPat (Pvar n') e an) = _eq cmpname n n'
                match n p                       = False
