|| mirandaExtensions.m -- extensions to Miranda's standard library


%export +

%import <maybe>


|| strict application
f $! x = x $seq f x

|| strict iteration
iterate' :: (* -> *) -> * -> [*]
iterate' f
    = go
      where
        go  a = a : fas a
        fas a = case f a of a' -> go a'

|| perform function f on both arguments of a binary operator, then perform the operator
|| e.g. "(*) $on fst" performs multiplication on the first part of two pairs
on :: (** -> ** -> ***) -> (* -> **) -> * -> * -> ***
on op f
    = go
      where
        go x y = f x $op f y


|| currying

curry :: ((*, **) -> ***) -> * -> ** -> ***
curry f = go where go a b = f (a, b)

uncurry :: (* -> ** -> ***) -> (*, **) -> ***
uncurry f = go where go (a, b) = f a b

curry3 :: ((*, **, ***) -> ****) -> * -> ** -> *** -> ****
curry3 f = go where go a b c = f (a, b, c)

uncurry3 :: (* -> ** -> *** -> ****) -> (*, **, ***) -> ****
uncurry3 f = go where go (a, b, c) = f a b c


|| bool functions

not :: bool -> bool
not x = ~x

xor :: bool -> bool -> bool
xor True  False = True
xor False True  = True
xor _     _     = False

if' :: bool -> * -> * -> *
if' True  a _ = a
if' False _ b = b


|| char functions

isLower :: char -> bool
isLower c = 'a' <=. c <=. 'z'

isUpper :: char -> bool
isUpper c = 'A' <=. c <=. 'Z'

toLower :: char -> char
toLower c
    = decode (code c - code 'A' + code 'a'), if isUpper c
    = c,                                     otherwise

toUpper :: char -> char
toUpper c
    = decode (code c - code 'a' + code 'A'), if isLower c
    = c,                                     otherwise

isSpace :: char -> bool
isSpace c
    = c ==. ' ' \/ c ==. '\t' \/ c ==. '\n'

digitVal :: char -> int
digitVal c
    = code c - code '0', if digit c
    = 0,                 otherwise


|| numeric functions

signum :: int -> int
signum n
    = case cmpint n 0 of
        EQ ->  0
        LT -> -1
        GT ->  1

even, odd :: int -> bool
even x = x .&. 1 == 0
odd  x = x .&. 1 == 1

gcd, lcm :: int -> int -> int
gcd x y
    = x,                if y == 0
    = gcd y (x $mod y), otherwise

lcm x y = (x * y) $div (gcd x y)


|| ordering functions

comparing, descending :: ordI ** -> (* -> **) -> ordI *
comparing  cmp f a b = cmp (f a) (f b)

descending cmp f a b
    = case comparing cmp f a b of
        EQ -> EQ
        LT -> GT
        GT -> LT


|| Tuple functions

cmptuple3 :: ordI * -> ordI ** -> ordI *** -> ordI (*, **, ***)
cmptuple3 cmpa cmpb cmpc
    = go
      where
        go (a0, b0, c0) (a1, b1, c1) = cmpa a0 a1 $thenCmp cmpb b0 b1 $thenCmp cmpc c0 c1

showtuple3 :: showI * -> showI ** -> showI *** -> showI (*, **, ***)
showtuple3 sa sb sc
    = go
      where
        go (a, b, c) = "(" ++ sa a ++ ", " ++ sb b ++ ", " ++ sc c ++ ")"

cmptuple4 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI (*, **, ***, ****)
cmptuple4 cmpa cmpb cmpc cmpd
    = go
      where
        go (a0, b0, c0, d0) (a1, b1, c1, d1) = cmpa a0 a1 $thenCmp cmpb b0 b1 $thenCmp cmpc c0 c1 $thenCmp cmpd d0 d1

showtuple4 :: showI * -> showI ** -> showI *** -> showI **** -> showI (*, **, ***, ****)
showtuple4 sa sb sc sd
    = go
      where
        go (a, b, c, d) = "(" ++ sa a ++ ", " ++ sb b ++ ", " ++ sc c ++ ", " ++ sd d ++ ")"

cmptuple5 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI (*, **, ***, ****, *****)
cmptuple5 cmpa cmpb cmpc cmpd cmpe
    = go
      where
        go (a0, b0, c0, d0, e0) (a1, b1, c1, d1, e1) = cmpa a0 a1 $thenCmp cmpb b0 b1 $thenCmp cmpc c0 c1 $thenCmp cmpd d0 d1 $thenCmp cmpe e0 e1

showtuple5 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI (*, **, ***, ****, *****)
showtuple5 sa sb sc sd se
    = go
      where
        go (a, b, c, d, e) = "(" ++ sa a ++ ", " ++ sb b ++ ", " ++ sc c ++ ", " ++ sd d ++ ", " ++ se e ++ ")"

cmptuple6 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI ****** -> ordI (*, **, ***, ****, *****, ******)
cmptuple6 cmpa cmpb cmpc cmpd cmpe cmpf
    = go
      where
        go (a0, b0, c0, d0, e0, f0) (a1, b1, c1, d1, e1, f1)
            = cmpa a0 a1 $thenCmp cmpb b0 b1 $thenCmp cmpc c0 c1 $thenCmp cmpd d0 d1 $thenCmp cmpe e0 e1 $thenCmp cmpf f0 f1

showtuple6 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI ****** -> showI (*, **, ***, ****, *****, ******)
showtuple6 sa sb sc sd se sf
    = go
      where
        go (a, b, c, d, e, f) = "(" ++ sa a ++ ", " ++ sb b ++ ", " ++ sc c ++ ", " ++ sd d ++ ", " ++ se e ++ ", " ++ sf f ++ ")"

cmptuple7 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI ****** -> ordI ******* ->
             ordI (*, **, ***, ****, *****, ******, *******)
cmptuple7 cmpa cmpb cmpc cmpd cmpe cmpf cmpg
    = go
      where
        go (a0, b0, c0, d0, e0, f0, g0) (a1, b1, c1, d1, e1, f1, g1)
            = cmpa a0 a1 $thenCmp cmpb b0 b1 $thenCmp cmpc c0 c1 $thenCmp cmpd d0 d1 $thenCmp cmpe e0 e1 $thenCmp cmpf f0 f1 $thenCmp cmpg g0 g1

showtuple7 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI ****** -> showI ******* ->
              showI (*, **, ***, ****, *****, ******, *******)
showtuple7 sa sb sc sd se sf sg
    = go
      where
        go (a, b, c, d, e, f, g) = "(" ++ sa a ++ ", " ++ sb b ++ ", " ++ sc c ++ ", " ++ sd d ++ ", " ++ se e ++ ", " ++ sf f ++ ", " ++ sg g ++ ")"

pair :: * -> ** -> (*, **)
pair a b = (a, b)

cmppair a b = cmptuple2 a b
showpair a  = showtuple2 a

triple :: * -> ** -> *** -> (*, **, ***)
triple a b c = (a, b, c)

cmptriple a b = cmptuple3 a b
showtriple a  = showtuple3 a

setFst :: (*, **) -> *** -> (***, **)
setFst (a, b) a' = (a', b)

setSnd :: (*, **) -> *** -> (*, ***)
setSnd (a, b) b' = (a, b')

mapFst :: (* -> ***) -> (*, **) -> (***, **)
mapFst f (a, b) = (f a, b)

mapSnd :: (** -> ***) -> (*, **) -> (*, ***)
mapSnd f (a, b) = (a, f b)

mapBoth :: (* -> **) -> (*, *) -> (**, **)
mapBoth f (a, b) = (f a, f b)

swapPair :: (*, **) -> (**, *)
swapPair (a, b) = (b, a)

dup :: * -> (*, *)
dup x = (x, x)

(&&&) :: (* -> **) -> (* -> ***) -> * -> (**, ***)
f &&& g = go where go x = (f x, g x)

(***) :: (* -> **) -> (*** -> ****) -> (*, ***) -> (**, ****)
f *** g = go where go (a, b) = (f a, g b)


|| list functions

null :: [*] -> bool
null [] = True
null _  = False

any, all :: (* -> bool) -> [*] -> bool
any p = foldr ((\/) . p) False
all p = foldr ((&) . p) True

elem :: ordI * -> * -> [*] -> bool
elem cmp = any . _eq cmp

find :: (* -> bool) -> [*] -> maybe *
find p
    = foldr match Nothing
      where
        match x k
            = Just x, if p x
            = k,      otherwise

count :: (* -> bool) -> [*] -> int
count p
    = foldl go 0
      where
        go n x = if' (p x) (n + 1) n

elemIndex :: ordI * -> * -> [*] -> maybe int
elemIndex cmp a
    = mb_fmap fst . find (_eq cmp a . snd) . enumerate

enumerate :: [*] -> [(int, *)]
enumerate = zip2 [0 ..]

isPrefixOf :: ordI * -> [*] -> [*] -> bool
isPrefixOf cmp
    = go
      where
        go []       _        = True
        go (x : xs) []       = False
        go (x : xs) (y : ys) = _eq cmp x y & go xs ys

stripPrefix :: ordI * -> [*] -> [*] -> maybe [*]
stripPrefix cmp
    = go
      where
        go []       ys       = Just ys
        go (x : xs) (y : ys) = go xs ys, if _eq cmp x y
        go _        _        = Nothing

unfoldr :: (** -> maybe (*, **)) -> ** -> [*]
unfoldr f
    = go
      where
        go b
            = fromMaybef [] cont (f b)
              where
                cont (a, b') = a : go b'

cycle :: [*] -> [*]
cycle xs = xs' where xs' = xs ++ xs'

singleton :: * -> [*]
singleton a = [a]

replicate :: int -> * -> [*]
replicate n = take n . repeat

delete :: ordI * -> * -> [*] -> [*]
delete cmp x
    = go
      where
        go [] = []
        go (y : ys)
            = ys,        if _eq cmp x y
            = y : go ys, otherwise

deleteAt :: int -> [*] -> [*]
deleteAt i xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = xs,                if n == 0
                      = x : go (n - 1) xs, otherwise

setAt :: int -> * -> [*] -> [*]
setAt i a xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = a : xs,            if n == 0
                      = x : go (n - 1) xs, otherwise

modifyAt :: int -> (* -> *) -> [*] -> [*]
modifyAt i f xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = f x : xs,          if n == 0
                      = x : go (n - 1) xs, otherwise

|| group adjacent elements according to a binary relation
|| note: this is different from Haskell's Prelude groupBy, which only performs the relation with the first element of the group
groupBy :: (* -> * -> bool) -> [*] -> [[*]]
groupBy p [] = []
groupBy p (x : xs)
    = (x : ys) : zs
      where
        (ys, zs) = go x xs
        go x (y : ys)
            = (y : as, bs),        if p x y
            = ([], (y : as) : bs), otherwise
              where (as, bs) = go y ys
        go x [] = ([], [])

group :: ordI * -> [*] -> [[*]]
group cmp = groupBy (_eq cmp)

concatMap :: (* -> [**]) -> [*] -> [**]
concatMap f xs = [y | x <- xs; y <- f x]

scanl :: (* -> ** -> *) -> * -> [**] -> [*]
scanl f z
    = go z
      where
        go z xs
            = z : case xs of
                    []     -> []
                    x : xs -> go (f z x) xs

scanr :: (** -> * -> *) -> * -> [**] -> [*]
scanr f z
    = go
      where
        go [] = [z]
        go (x : xs)
            = f x (hd qs) : qs
              where
                qs = go xs

inits, tails :: [*] -> [[*]]
inits xs = (map reverse . scanl (converse (:)) []) xs

tails [] = []
tails xs = xs : tails (tl xs)

mapAccumL :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
mapAccumL f s
    = go s
      where
        go s [] = (s, [])
        go s (x : xs)
            = (s2, x' : xs')
              where
                (s1, x')  = f s x
                (s2, xs') = go s1 xs

mapAccumR :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
mapAccumR f s
    = go s
      where
        go s [] = (s, [])
        go s (x : xs)
            = (s2, x' : xs')
              where
                (s1, xs') = go s xs
                (s2, x')  = f s1 x

takeWhile :: (* -> bool) -> [*] -> [*]
takeWhile f
    = go
      where
        go [] = []
        go (x : xs) = x : go xs, if f x
                    = [],        otherwise

dropWhile :: (* -> bool) -> [*] -> [*]
dropWhile f
    = go
      where
        go [] = []
        go xs = go (tl xs), if f (hd xs)
              = xs,         otherwise

partition :: (* -> bool) -> [*] -> ([*], [*])
partition test
    = foldr part ([], [])
      where
        part x k                        || note: k is evaluated lazily instead of pattern matched, which would
            = (x : as, bs), if test x   || force its evaluation
            = (as, x : bs), otherwise
              where
                (as, bs) = k

|| heavily used in parsing, so optimized with strict case evaluation
span, break :: (* -> bool) -> [*] -> ([*], [*])
span p
    = go
      where
        go xs
            = case xs of
                []      -> finish
                x : xs' -> case p x of
                             False -> finish
                             True  -> case go xs' of (ys, zs) -> (x : ys, zs)
              where
                finish = ([], xs)

break p = span (not . p)

viewL :: [*] -> maybe (*, [*])
viewL []       = Nothing
viewL (x : xs) = Just (x, xs)

viewR :: [*] -> maybe ([*], *)
viewR
    = foldr go Nothing
      where
        go x Nothing         = Just ([], x)
        go x (Just (xs, x')) = Just (x : xs, x')

|| originally written with pattern matching for recursive splitAt result and hd:tl for xs, but this
|| way turns out to be twice as fast...
splitAt :: int -> [*] -> ([*], [*])
splitAt n xs
    = case n <= 0 of
        False -> case xs of
                   []      -> ([], [])
                   x : xs' -> merge x $ splitAt (n - 1) xs'
        True  -> ([], xs)
      where
        merge x (ys, zs) = (x : ys, zs)

|| split a list into 2 approximately equal halves
split2 :: [*] -> ([*], [*])
split2 xs
    = go xs xs
      where
        go (x : xs) (y : y' : ys)
            = merge x $ go xs ys
        go xs _ = ([], xs)

        merge x (ys, zs) = (x : ys, zs)

|| split a list into sublists, using a predicate function to test list entries,
|| and removing any empty entries (words == splitWhen isSpace)
splitWhen :: (* -> bool) -> [*] -> [[*]]
splitWhen p
    = go
      where
        go xs
            = merge y (rest ys)
              where
                (y, ys) = break p xs

                merge [] ys   = ys
                merge y  ys   = y : ys

                rest []       = []
                rest (_ : ys) = go ys

splitOneOf :: ordI * -> [*] -> [*] -> [[*]]
splitOneOf cmp xs = splitWhen (member cmp xs)

interleave :: [*] -> [*] -> [*]
interleave (a : as) (b : bs) = a : b : interleave as bs
interleave _        _        = []

uninterleave :: [*] -> ([*], [*])
uninterleave xs
    = foldr ui ([], []) xs
      where
        ui x (xs, ys) = (x : ys, xs)

chunk :: int -> [*] -> [[*]]
chunk n xs
    = error ("chunk: bad chunk size: " ++ showint n), if n < 1
    = go xs,                                          otherwise
      where
        go xs = case splitAt n xs of
                  (ys, zs) -> case zs of
                                [] -> [ys]
                                _  -> ys : go zs

nub :: ordI * -> [*] -> [*]
nub cmp [] = []
nub cmp (x : xs) = x : nub cmp (filter (_ne cmp x) xs)

length :: [*] -> int
length xs = #xs

intersperse :: * -> [*] -> [*]
intersperse x [] = []
intersperse x (y : ys)
    = y : intersperse' ys
      where
        intersperse' [] = []
        intersperse' (y : ys) = x : y : intersperse' ys

intercalate :: [*] -> [[*]] -> [*]
intercalate xs = concat . intersperse xs

|| combinations of N items taken from a list (no repetitions, ordering doesn't matter)
|| e.g. combinations 2 "abc" = ["ab", "ac", "bc"]
combinations :: int -> [*] -> [[*]]
combinations 0 _        = [[]]
combinations n []       = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

|| combinations of N items taken from a list (with repetitions, ordering doesn't matter)
|| e.g. combinationsWithRep 2 "abc" = ["aa", "ab", "ac", "bb", "bc", "cc"]
combinationsWithRep :: int -> [*] -> [[*]]
combinationsWithRep 0 _  = [[]]
combinationsWithRep n [] = []
combinationsWithRep n xs = map ((hd xs) :) (combinationsWithRep (n - 1) xs) ++ combinationsWithRep n (tl xs)

|| permutations of a list (no repetitions, ordering matters)
|| e.g. permutations "abc" == ["abc", "acb", "bac", "bca", "cab", "cba"]
permutations :: [*] -> [[*]]
permutations
    = go []
      where
        go [] []       = [[]]
        go ys []       = []
        go ys (x : xs) = map (x :) (permutations (ys ++ xs)) ++ (go (x : ys) xs)

|| permutations of a list (with repetitions, ordering matters)
|| e.g. permutationsWithRep "abc" = ["aaa", "aab", "aac", "aba", "abb", "abc", "aca", "acb", "acc", "baa" ..]
permutationsWithRep :: [*] -> [[*]]
permutationsWithRep xs
    = go xs     || use list as permutation length counter to determine when to stop recursion
      where
        go []       = [[]]
        go (y : ys) = [x : ps | x <- xs; ps <- go ys]

|| Note: ensures that the mapping function is only applied
|| once to each element, as it could be expensive
minBy :: ordI ** -> (* -> **) -> [*] -> *
minBy cmp f
    = snd . foldl1 selMin . map addOrder
      where
        addOrder x = case f x of y -> (y, x)
        selMin a b
            = a, if _le cmp (fst a) (fst b)
            = b, otherwise

|| Note: ensures that the mapping function is only applied
|| once to each element, as it could be expensive
maxBy :: ordI ** -> (* -> **) -> [*] -> *
maxBy cmp f
    = snd . foldl1 selMax . map addOrder
      where
        addOrder x = case f x of y -> (y, x)
        selMax a b
            = a, if _ge cmp (fst a) (fst b)
            = b, otherwise

sortBy :: ordI * -> [*] -> [*]
sortBy cmp
    = mergeAll . sequences
      where
        sequences (a : b : xs)
            = descending b [a]   xs, if _gt cmp a b
            = ascending  b (a :) xs, otherwise
        sequences xs = [xs]

        descending a as (b : bs) = descending b (a : as) bs, if _gt cmp a b
        descending a as bs       = (a : as) : sequences bs

        ascending a fa (b : bs)
            = ascending b fa' bs, if _le cmp a b
              where fa' ys = fa (a : ys)

        ascending a fa bs = case fa [a] of x -> (x : sequences bs)

        mergeAll [x] = x
        mergeAll xs  = mergeAll (mergePairs xs)

        mergePairs (a : b : xs) = case merge a b of x -> (x : mergePairs xs)
        mergePairs xs           = xs

        merge [] bs = bs
        merge as [] = as
        merge as bs
            = b : merge as bs', if _gt cmp a b
            = a : merge as' bs, otherwise
              where
                (a : as') = as
                (b : bs') = bs

|| Note: ensures that the mapping function is only applied
|| once to each element, as it could be expensive
sortOn :: ordI ** -> (* -> **) -> [*] -> [*]
sortOn cmp f
    = map snd . sortBy (comparing cmp fst) . map addOrder
      where
        addOrder x = case f x of y -> (y, x)

zipWith :: (* -> ** -> ***) -> [*] -> [**] -> [***]
zipWith fn 
    = go
      where
        go (x : xs) (y : ys) = fn x y : go xs ys
        go _        _        = []

unzip2 :: [(*, **)] -> ([*], [**])
unzip2 [] = ([], [])
unzip2 ((a, b) : ts) = (a : as, b : bs) where (as, bs) = unzip2 ts

unzip3 :: [(*, **, ***)] -> ([*], [**], [***])
unzip3 [] = ([], [], [])
unzip3 ((a, b, c) : ts) = (a : as, b : bs, c : cs) where (as, bs, cs) = unzip3 ts


|| string functions

|| split a string into substrings, separated by the specified character, but don't remove
|| empty substrings (lines == split '\n')
split :: char -> string -> [string]
split c
    = foldr go []
      where
        go x r        = [] : r, if x ==. c
        go x []       = [x] : []
        go x (xs : t) = (x : xs) : t

words :: string -> [string]
words s = splitWhen isSpace s

|| add a suffix to a string if it isn't already present
withSuffix :: string -> string -> string
withSuffix suf str
    = str,                              if str ==$ dotSuf
    = str ++ suf,                       if str ==$ "."
    = str ++ dotSuf,                    if #str <= 2
    = hd str : withSuffix suf (tl str), otherwise
      where
        dotSuf = '.' : suf

|| remove a suffix from a string, if it is present
withoutSuffix :: string -> string -> string
withoutSuffix suf str
    = [],                                 if null str \/ str ==$ dotSuf
    = hd str : withoutSuffix suf (tl str), otherwise
      where
        dotSuf = '.' : suf
