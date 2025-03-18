|| trieMap -- strict map from a key to a value, where the key is a list of elements
|| this has higher performance than a regular map if the keys have common prefixes
|| implemented using Patricia tries, which are prefix tries with variable-sized prefixes


%export +

%import <either>
%import <maybe>                 (>>=?)/mb_bind (<$>?)/mb_fmap (<|>?)/mb_alt
%import <mirandaExtensions>


|| a trieBranch holds a prefix key and its associated sub-trie
trieBranch * ** == ([*], trie * **)     

|| result of matching a key with a trieBranch 
matchResult * ** ::=
    Mfail                               |
    Mkey  (trie * **) [*] [*]           |  || match full prefix -- match, remaining key
    Mpre  (trie * **) [*] [*]           |  || match full key    -- match, remaining prefix
    Mpart (trie * **) [*] [*] [*]          || partial match     -- match,  remaining key, remaining prefix

|| compare the key with the prefix key stored in the trieBranch
tb_match :: ordI * -> [*] -> trieBranch * ** -> matchResult * **
tb_match cmp ks (ps, t)
    = go ks ps []
      where
        go (k : ks) (p : ps) m = go ks ps (k : m), if _eq cmp k p
        go k p m
            = Mfail,                    if null m
            = Mkey  t k  ps,            if null p
            = Mpre  t ks p,             if null m
            = Mpart t k  p (reverse m), otherwise

|| find a trieBranch prefix-matching the key
tb_find :: ordI * -> [*] -> [trieBranch * **] -> matchResult * **
tb_find cmp k bs
    = foldr find Mfail bs
      where
        find b cont
            = case tb_match cmp k b of
                Mfail -> cont
                r     -> r

|| modify a trieBranch prefix-matching the key with the supplied function, and return the modified trieBranch list
|| return Nothing if no prefix-match found
|| the modification function returns a maybe trieBranch: if it returns Nothing, then the match and modification fails
tb_modify :: ordI * -> (matchResult * ** -> maybe (trieBranch * **)) -> [*] -> [trieBranch * **] -> maybe [trieBranch * **]
tb_modify cmp check k bs
    = go k bs
      where
        go k [] = Nothing                       || no match found
        go k (b : bs)
            = case check $ tb_match cmp k b of
                Nothing -> (b :) <$>? go k bs   || branch didn't match or failed check; merge this branch with the result of checking the remaining
                Just b' -> Just (b' : bs)       || branch passed check; return modified branch along with remaining unexamined branches

|| a trie is a list of trieBranches to continue prefix matching and a possible value stored at this trie level
trie * ** ::= Trie [trieBranch * **] (maybe **)

t_empty :: trie * **
t_empty = Trie [] Nothing

t_null :: trie * ** -> bool
t_null (Trie [] _) = True
t_null _           = False

|| lookup the value for a key, returning Nothing if the key is not present
t_lookup :: ordI * -> [*] -> trie * ** -> maybe **
t_lookup cmp k t
    = go k t
      where
        go [] (Trie bs mv) = mv
        go k  (Trie bs mv)
            = case tb_find cmp k bs of
                Mkey t k _ -> go k t    || matched full prefix; continue with remaining key and matching trieBranch
                _          -> Nothing   || no match for all other cases

t_findWithDefault :: ordI * -> ** -> [*] -> trie * ** -> **
t_findWithDefault cmp d xs t
    = fromMaybe d $ t_lookup cmp xs t

|| lookup the first value that matches a prefix of the key
t_prefix :: ordI * -> [*] -> trie * ** -> maybe **
t_prefix cmp k t
    = go k t
      where
        go [] (Trie bs mv) = mv
        go k  (Trie bs mv) = mv <|>? lookup k bs

        lookup k bs
            = case tb_find cmp k bs of
                Mkey t k _ -> go k t    || matched full prefix; continue with remaining key and matching trieNBranch
                _          -> Nothing

|| insert a key-value pair
t_insert :: ordI * -> [*] -> ** -> trie * ** -> trie * **
t_insert cmp k v t
    = go k t
      where
        tNew bs = v $seq Trie bs (Just v)               || create a new Trie with an existing trieBranch list and the value

        go [] (Trie bs mv) = tNew bs                    || trie matches key -- replace value
        go k  (Trie bs mv)
            = case tb_modify cmp check k bs of
                Nothing  -> b' $seq Trie (b' : bs) mv   || no matching trieBranch -- insert a new one
                Just bs' -> Trie bs' mv                 || partial match found -- replace branches
              where
                b' = case tNew [] of t' -> (k, t')

        check Mfail           = Nothing
        check (Mkey  t k p)   = case go k t        of t' -> Just (p, t')                                || continue matching k, insert under p
        check (Mpre  t k p)   = case tNew [(p, t)] of t' -> Just (k, t')                                || insert remaining p under new trieBranch k
        check (Mpart t k p m) = case tNew []       of t' -> Just (m, Trie [(k, t'), (p, t)] Nothing)    || split, insert remaining k and p under new

|| insert a key-value pair using supplied insertion function
t_insertWith :: ordI * -> (** -> ** -> **) -> [*] -> ** -> trie * ** -> trie * **
t_insertWith cmp f k v t
    = go k t
      where
        tNew bs = v $seq Trie bs (Just v)               || create a new Trie with an existing trieBranch list and the value

        go [] (Trie bs mv)
            = case mv of
                Nothing -> tNew bs
                Just v' -> case f v v' of fv' -> Trie bs (Just fv')

        go k  (Trie bs mv)
            = case tb_modify cmp check k bs of
                Nothing  -> b' $seq Trie (b' : bs) mv   || no matching trieBranch -- insert a new one
                Just bs' -> Trie bs' mv                 || partial match found -- replace branches
              where
                b' = case tNew [] of t' -> (k, t')

        check Mfail           = Nothing
        check (Mkey  t k p)   = case go k t        of t' -> Just (p, t')                                || continue matching k, insert under p
        check (Mpre  t k p)   = case tNew [(p, t)] of t' -> Just (k, t')                                || insert remaining p under new trieBranch k
        check (Mpart t k p m) = case tNew []       of t' -> Just (m, Trie [(k, t'), (p, t)] Nothing)    || split, insert remaining k and p under new

|| delete a key-value pair in the trieMap (simply replace the value with Nothing, leaving the key intact)
t_delete :: ordI * -> [*] -> trie * ** -> trie * **
t_delete cmp k t
    = fromMaybe t $ go k t
      where
        go [] (Trie bs mv) = Just $ Trie bs Nothing     || trie matches key -- remove value
        go k  (Trie bs mv) = converse Trie mv <$>? tb_modify cmp check k bs

        check (Mkey t k p) = pair p <$>? go k t         || continue matching k, return modified trieBranch if a deletion occurred
        check _            = Nothing

t_adjust :: ordI * -> (** -> **) -> [*] -> trie * ** -> trie * **
t_adjust cmp f k t
    = fromMaybe t $ go k t
      where
        go [] (Trie bs mv) = (Trie bs . Just . f) <$>? mv       || trie matches key -- perform adjustment if value exists
        go k  (Trie bs mv) = converse Trie mv <$>? tb_modify cmp check k bs

        check (Mkey t k p) = pair p <$>? go k t         || continue matching k, return modified trieBranch if an adjust occurred
        check _            = Nothing

|| filter the elements of a trieMap with a predicate function and return the filtered trieMap
|| this can be done by simply deleting the elements that don't pass the predicate function, keeping the same
|| trie structure
t_filter :: (** -> bool) -> trie * ** -> trie * **
t_filter f t
    = go t
      where
        go (Trie bs mv)
            = case map goNext bs of
                bs' -> case mv of
                         Nothing -> Trie bs' Nothing
                         Just v  -> case f v of
                                      False -> Trie bs' Nothing
                                      True  -> Trie bs' (Just v)
        goNext (p, t) = case go t of t' -> (p, t')

t_union :: ordI * -> trie * ** -> trie * ** -> trie * **
t_union cmp t1 t2
    = foldl ins t1 $ t_toList t2
      where
        ins t (k, v) = t_insert cmp k v t

||    = go [] t1 t2
||      where
        go p1 t1 (Trie bs mv)
            = foldl (goNext p1) (fromMaybef t1 ins mv) bs
              where
                ins v = t_insert cmp p1 v t1

                goNext p1 t1 (p2, t2) = go (p1 ++ p2) t1 t2

t_toList :: trie * ** -> [([*], **)]
t_toList t
    = go [] t
      where
        go p1 (Trie bs mv)
            = case mv of
                Nothing -> next
                Just v  -> (p1, v) : next
              where
                next = concatMap goNext bs

                goNext (p2, t') = go (p1 ++ p2) t'

t_fromList :: ordI * -> [([*], **)] -> trie * **
t_fromList cmp
    = foldl ins t_empty
      where
        ins t (k, v) = t_insert cmp k v t

t_keys :: trie * ** -> [[*]]
t_keys t = map fst $ t_toList t

t_elems :: trie * ** -> [**]
t_elems t = map snd $ t_toList t


|| functor interace
t_fmap :: (** -> ***) -> trie * ** -> trie * ***
t_fmap f t
    = go t
      where
        go (Trie bs mv) = case map goNext bs of bs' -> case f <$>? mv of mv' -> Trie bs' mv'
        goNext (p, t)   = case go t of t' -> (p, t')


|| foldable interace
t_foldr :: (** -> *** -> ***) -> *** -> trie * ** -> ***
t_foldr f z
    = go z
      where
        go z' (Trie bs mv) = foldr ftb z' bs |> check mv

        ftb (p, t) z' = go z' t

        check Nothing z'   = z'
        check (Just v) z'  = f v z'

t_foldl :: (*** -> ** -> ***) -> *** -> trie * ** -> ***
t_foldl f z
    = go z
      where
        go z' (Trie bs mv) = fromMaybef z' (f z') mv |> converse (foldl ftb) bs

        ftb z' (p, t) = go z' t
