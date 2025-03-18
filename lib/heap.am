|| heap.m -- tree-based priority queue


%export heap h_empty h_null h_size h_singleton h_insert h_union h_viewMin h_fromList h_toList showheap

%import <maybe>


htree *   ::= HTnode int! * (hforest *)      || rank, root, hforest
hforest * == [htree *]                       || list of htrees, ordered from lowest to highest root value based upon cmp fn

rank :: htree * -> int
rank (HTnode r _ _) = r

root :: htree * -> *
root (HTnode _ x _) = x

link :: ordI * -> htree * -> htree * -> htree *
link cmp t1 t2
    = HTnode (r1 + 1) x1 (t2 : ts1), if _le cmp x1 x2
    = HTnode (r2 + 1) x2 (t1 : ts2), otherwise
      where
        HTnode r1 x1 ts1 = t1
        HTnode r2 x2 ts2 = t2

skewLink :: ordI * -> htree * -> htree * -> htree * -> htree *
skewLink cmp t0 t1 t2
    = case cmp x2 x1 of
        || x2 < x1; compare x2 to x0
        LT -> case cmp x2 x0 of
                LT -> HTnode (r2 + 1) x2 (t0 : t1 : ts2)
                _  -> HTnode (r1 + 1) x0 (t1 : t2 : ts0)
        || x1 <= x2; compare x1 to x0
        _ -> case cmp x1 x0 of
                LT -> HTnode (r1 + 1) x1 (t0 : t2 : ts1)
                _  -> HTnode (r1 + 1) x0 (t1 : t2 : ts0)
      where
        HTnode _  x0 ts0 = t0
        HTnode r1 x1 ts1 = t1
        HTnode r2 x2 ts2 = t2

ins :: ordI * -> htree * -> hforest * -> hforest *
ins cmp t [] = [t]
ins cmp t (t' : ts)
    = t : t' : ts,                if rank t < rank t'
    = ins cmp (link cmp t t') ts, otherwise

uniqify :: ordI * -> hforest * -> hforest *
uniqify cmp []       = []
uniqify cmp (t : ts) = ins cmp t ts

unionUniq :: ordI * -> hforest * -> hforest * -> hforest *
unionUniq _ [] ys = ys
unionUniq _ xs [] = xs
unionUniq cmp (x : xs) (y : ys)
    = case cmpint (rank x) (rank y) of
        EQ -> ins cmp (link cmp x y) (unionUniq cmp xs ys)
        LT -> x : unionUniq cmp xs (y : ys)
        GT -> y : unionUniq cmp (x : xs) ys

unionUniq _ _ _ = undef || added to remove compiler warning

skewInsert :: ordI * -> htree * -> hforest * -> hforest *
skewInsert cmp t (t1 : t2 : ts)
    = skewLink cmp t t1 t2 : ts, if rank t1 == rank t2
    = t : t1 : t2 : ts,          otherwise

skewInsert _ t ts = t : ts

skewMeld :: ordI * -> hforest * -> hforest * -> hforest *
skewMeld cmp ts1 ts2 = unionUniq cmp (uniqify cmp ts1) (uniqify cmp ts2)

|| extract the tree with the minimum root value from a forest, leaving the order of the
|| rest of the trees in the forest intact
viewMin :: ordI * -> hforest * -> maybe (htree *, hforest *)
viewMin cmp [] = Nothing
viewMin cmp (t : ts)
    = Just $ go t ts
      where
        go t1 [] = (t1, [])
        go t1 (t2 : ts)
            = case go t2 ts of
                (mt, ts') -> case cmp (root t1) (root mt) of
                               GT -> (mt, t1 : ts')
                               _  -> (t1, t2 : ts)

splitForest :: int -> hforest * -> hforest * -> hforest * -> (hforest *, hforest *, hforest *)
splitForest 0 xs ys ts = (xs, ys, ts)

splitForest 1 xs ys [t] = (xs, t : ys, [])
splitForest 1 xs ys (t1 : t2 : ts)
    = (t1 : xs, t2 : ys, ts), if rank t2 == 0
    = (xs, t1 : ys, t2 : ts), otherwise

splitForest r xs ys (t1 : t2 : ts)
    = (xs, t1 : t2 : ys, ts),                     if r1 == r2
    = splitForest (r - 1) (t1 : xs) (t2 : ys) ts, if r1 == 0
    = splitForest (r - 1) xs (t1 : ys) (t2 : ts), otherwise
      where
        r1 = rank t1
        r2 = rank t2

splitForest r xs ys ts = error "heap splitForest: invalid arguments"

t_toList :: htree * -> [*]
t_toList (HTnode _ x ts)
    = x : f_toList ts
      where
        f_toList []       = []
        f_toList (t : ts) = t_toList t ++ f_toList ts


heap * ::= Hempty | Heap int (htree *)

h_empty :: heap *
h_empty = Hempty

h_null :: heap * -> bool
h_null Hempty = True
h_null h      = False

h_size :: heap * -> int
h_size Hempty     = 0
h_size (Heap s t) = s

h_singleton :: * -> heap *
h_singleton x = Heap 1 (HTnode 0 x [])

h_insert :: ordI * -> * -> heap * -> heap *
h_insert cmp x Hempty = h_singleton x
h_insert cmp x (Heap s t)
    = Heap s' (HTnode 0 x [t]),                                 if _le cmp x y
    = Heap s' (HTnode 0 y (skewInsert cmp (HTnode 0 x []) ts)), otherwise
      where
        HTnode _ y ts = t
        s' = case s + 1 of x -> x

h_union :: ordI * -> heap * -> heap * -> heap *
h_union cmp Hempty q      = q
h_union cmp q      Hempty = q
h_union cmp (Heap s1 t1) (Heap s2 t2)
    = Heap s' (HTnode 0 x1 (skewInsert cmp t2 ts1)), if _le cmp x1 x2
    = Heap s' (HTnode 0 x2 (skewInsert cmp t1 ts2)), otherwise
      where
        HTnode _ x1 ts1 = t1
        HTnode _ x2 ts2 = t2

        s' = case s1 + s2 of x -> x

h_union _ _ _ = undef     || added to prevent spurious non-exhaustive pattern warning

h_viewMin :: ordI * -> heap * -> maybe (*, heap *)
h_viewMin cmp Hempty = Nothing
h_viewMin cmp (Heap s (HTnode r x ts))
    = case viewMin cmp ts of
        Nothing -> Just (x, Hempty)
        Just r  -> case go r of (x', ts') -> Just (x, Heap (s - 1) (HTnode 0 x' ts'))
      where
        go (HTnode r x ts, ts')
            = case splitForest r [] [] ts of
                (xs, ys, zs) -> case skewMeld cmp (skewMeld cmp ys ts') zs of
                                  ts2 -> case foldr (skewInsert cmp) ts2 xs of
                                           ts3 -> (x, ts3)

h_fromList :: ordI * -> [*] -> heap *
h_fromList cmp = foldr (h_insert cmp) h_empty

h_toList :: heap * -> [*]
h_toList Hempty = []
h_toList (Heap _ t) = t_toList t
