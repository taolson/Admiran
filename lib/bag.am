|| bag.m -- implementation of a strict multiset using AVL trees


%export + b_empty b_null b_size b_member b_first b_last b_lookup b_findWithDefault b_elems b_keys b_keysSet b_toList b_deleteKey b_madjust b_adjust

%import <avl>
%import <map>   b_empty/m_empty b_null/m_null b_size/m_size b_member/m_member b_first/m_first b_last/m_last b_lookup/m_lookup
                b_findWithDefault/m_findWithDefault b_elems/m_elems b_keys/m_keys b_keysSet/m_keysSet b_toList/m_toList
                b_deleteKey/m_delete b_madjust/m_madjust b_adjust/m_adjust

%import <maybe> (<$>?)/mb_fmap


b_bag * == m_map * int

b_singleton :: * -> b_bag *
b_singleton x = m_singleton x 1

b_insert :: ordI * -> * -> b_bag * -> b_bag *
b_insert cmp key = m_insertWith cmp (+) key 1

b_insertTimes :: ordI * -> * -> int -> b_bag * -> b_bag *
b_insertTimes cmp = m_insertWith cmp (+)

b_delete :: ordI * -> * -> b_bag * -> b_bag *
b_delete cmp key = b_deleteTimes cmp key 1

b_deleteTimes :: ordI * -> * -> int -> b_bag * -> b_bag *
b_deleteTimes cmp k n b
    = fromMaybe b $ go b
      where
        go AVLLeaf = Nothing
        go (AVLNode p l r h)
            = case p of
                (k', n') -> case cmp k k' of
                              EQ -> case nd > 0 of
                                      False -> Just $ a_moveR l r
                                      True  -> Just $ AVLNode (k, nd) l r h
                              LT -> insL <$>? go l
                              GT -> insR <$>? go r
                            where
                              nd = n' - n
              where
                insL l' = a_balance $ AVLNode p l' r h
                insR r' = a_balance $ AVLNode p l r' h


b_fromList :: ordI * -> [*] -> b_bag *
b_fromList cmp = foldl (converse (b_insert cmp)) b_empty

b_fromCountList :: ordI * -> [(*, int)] -> b_bag *
b_fromCountList cmp
    = foldl addEntry b_empty
      where
        addEntry b (k, v) = b_insertTimes cmp k v b

|| make a bag from a list of keys, with zero counts
b_withKeys :: ordI * -> [*] -> b_bag *
b_withKeys cmp
    = foldl ins b_empty
      where
        ins b k = b_insertTimes cmp k 0 b

b_union :: ordI * -> b_bag * -> b_bag * -> b_bag *
b_union cmp b1 b2
    = foldl addEntry b1 (b_toList b2)
      where
        addEntry b (k, v) = b_insertTimes cmp k v b
