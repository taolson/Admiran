|| set.m -- implementation of a strict set using AVL trees


%export + s_empty s_null s_singleton s_size s_member s_first s_last s_delete s_toList s_foldr s_foldl

%import <avl>   s_empty/a_empty s_null/a_null s_singleton/a_singleton s_size/a_size s_member/a_member
                s_first/a_first s_last/a_last s_delete/a_delete s_toList/a_toList s_foldr/a_foldr s_foldl/a_foldl
%import <maybe> (<$>?)/mb_fmap

s_set * == avlTree *

s_insert :: ordI * -> * -> s_set * -> s_set *
s_insert cmp v
    = go
      where
        go  AVLLeaf = s_singleton v
        go (AVLNode v' l r h)
            = case cmp v v' of
                EQ -> AVLNode v l r h                   || same value maps to single node
                LT -> case go l of l' -> a_balance $ AVLNode v' l' r h
                GT -> case go r of r' -> a_balance $ AVLNode v' l r' h

|| insert a value into the set only if it is not already present;
|| otherwise return Nothing
s_insertUnique :: ordI * -> * -> s_set * -> maybe (s_set *)
s_insertUnique cmp v
    = go
      where
        go  AVLLeaf = Just $ s_singleton v
        go (AVLNode v' l r h)
            = case cmp v v' of
                EQ -> Nothing
                LT -> insL <$>? go l
                GT -> insR <$>? go r
              where
                insL l' = a_balance $ AVLNode v' l' r h
                insR r' = a_balance $ AVLNode v' l r' h

|| return the minimum element and the rest of the set, or Nothing if the set is empty
s_viewMin :: s_set * -> maybe (*, s_set *)
s_viewMin AVLLeaf = Nothing
s_viewMin (AVLNode v l r h)
    = case s_null l of
        False -> balanceL <$>? s_viewMin l
        True  -> Just (v, r)
      where
        balanceL (v', l') = case a_balance $ AVLNode v l' r h of n' -> (v', n')

|| return the maximum element and the rest of the set, or Nothing if the set is empty
s_viewMax :: s_set * -> maybe (*, s_set *)
s_viewMax AVLLeaf = Nothing
s_viewMax (AVLNode v l r h)
    = case s_null r of
        False -> balanceR <$>? s_viewMax r
        True  -> Just (v, l)
      where
        balanceR (v', r') = case a_balance $ AVLNode v l r' h of n' -> (v', n')

|| find the closest value in the set that is <= the given value
s_lookupLE :: ordI * -> * -> s_set * -> maybe *
s_lookupLE cmp v
    = go Nothing
      where
        go b AVLLeaf = b
        go b (AVLNode v' l r h)
            = case cmp v' v of
                EQ -> Just v'
                LT -> go (Just v') r
                GT -> go b l

|| find the closest value in the set that is >= the given value
s_lookupGE :: ordI * -> * -> s_set * -> maybe *
s_lookupGE cmp v
    = go Nothing
      where
        go b AVLLeaf = b
        go b (AVLNode v' l r h)
            = case cmp v' v of
                EQ -> Just v'
                LT -> go b r
                GT -> go (Just v') l

s_fromList :: ordI * -> [*] -> s_set *
s_fromList cmp = foldr (s_insert cmp) s_empty

s_union :: ordI * -> s_set * -> s_set * -> s_set *
s_union cmp s1 s2
    = foldl ins s1 (s_toList s2)
      where
        ins s v = s_insert cmp v s 

s_difference :: ordI * -> s_set * -> s_set * -> s_set *
s_difference cmp s1 s2
    = foldl del s1 (s_toList s2)
      where
        del s v = s_delete cmp v s 

s_intersect :: ordI * -> s_set * -> s_set * -> s_set *
s_intersect cmp s1 s2
    = s_fromList cmp (go (s_toList s1) (s_toList s2))
      where
        go (x : xs) (y : ys)
            = case cmp x y of
                EQ -> x : go xs       ys
                LT ->     go xs       (y : ys)
                GT ->     go (x : xs) ys
        go xs ys = []

s_filter :: ordI * -> (* -> bool) -> s_set * -> s_set *
s_filter cmp p = s_fromList cmp . filter p . s_toList


|| functor interface
|| s_fmap changes the key value that is used in the AVL tree, so the easiest
|| way to do it is to serialize to a list, map, then deserialize to the new set
s_fmap :: ordI ** -> (* -> **) -> s_set * -> s_set **
s_fmap cmp f = s_fromList cmp . map f . s_toList
