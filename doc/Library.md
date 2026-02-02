# Admiran Library Modules

This file documents the exported module interface for each of the modules in the Admiran library,
including Data definitions, type synonyms, and definition type specs

### avl
    avl.avlTree * ::= avl.AVLLeaf | avl.AVLNode * (avl.avlTree *) (avl.avlTree *) stdlib.int
    avl.AVLLeaf :: avl.avlTree *
    avl.AVLNode :: * -> avl.avlTree * -> avl.avlTree * -> stdlib.int -> avl.avlTree *
    avl.a_balance :: avl.avlTree * -> avl.avlTree *
    avl.a_delete :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> avl.avlTree *
    avl.a_empty :: avl.avlTree *
    avl.a_first :: avl.avlTree * -> *
    avl.a_fmap :: (** -> ** -> stdlib.ordering) -> (* -> **) -> avl.avlTree * -> avl.avlTree **
    avl.a_foldl :: (** -> * -> **) -> ** -> avl.avlTree * -> **
    avl.a_foldr :: (* -> ** -> **) -> ** -> avl.avlTree * -> **
    avl.a_fromList :: (* -> * -> stdlib.ordering) -> [*] -> avl.avlTree *
    avl.a_insert :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> avl.avlTree *
    avl.a_last :: avl.avlTree * -> *
    avl.a_member :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> stdlib.bool
    avl.a_moveR :: avl.avlTree * -> avl.avlTree * -> avl.avlTree *
    avl.a_null :: avl.avlTree * -> stdlib.bool
    avl.a_singleton :: * -> avl.avlTree *
    avl.a_size :: avl.avlTree * -> stdlib.int
    avl.a_toList :: avl.avlTree * -> [*]
    avl.a_union :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> avl.avlTree *
    avl.cmpavlTree :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> stdlib.ordering
    avl.showavlTree :: (* -> [stdlib.char]) -> avl.avlTree * -> [stdlib.char]

### bag
    bag.b_bag * == avl.avlTree ((*, stdlib.int))
    bag.b_delete :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int))
    bag.b_deleteTimes :: (* -> * -> stdlib.ordering) -> * -> stdlib.int -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int))
    bag.b_fromCountList :: (* -> * -> stdlib.ordering) -> [(*, stdlib.int)] -> avl.avlTree ((*, stdlib.int))
    bag.b_fromList :: (* -> * -> stdlib.ordering) -> [*] -> avl.avlTree ((*, stdlib.int))
    bag.b_insert :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int))
    bag.b_insertTimes :: (* -> * -> stdlib.ordering) -> * -> stdlib.int -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int))
    bag.b_singleton :: * -> avl.avlTree ((*, stdlib.int))
    bag.b_union :: (* -> * -> stdlib.ordering) -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int))
    bag.b_withKeys :: (* -> * -> stdlib.ordering) -> [*] -> avl.avlTree ((*, stdlib.int))
    bag.cmpb_bag :: (* -> * -> stdlib.ordering) -> avl.avlTree ((*, stdlib.int)) -> avl.avlTree ((*, stdlib.int)) -> stdlib.ordering
    bag.showb_bag :: (* -> [stdlib.char]) -> avl.avlTree ((*, stdlib.int)) -> [stdlib.char]

### base
    (base.!>) :: * -> (* -> **) -> **
    (base.$!) :: (* -> **) -> * -> **
    (base.&&&) :: (* -> **) -> (* -> ***) -> * -> (**, ***)
    (base.***) :: (* -> **) -> (*** -> ****) -> (*, ***) -> (**, ****)
    (base.|*|) :: [*] -> [**] -> [(*, **)]
    base.all :: (* -> stdlib.bool) -> [*] -> stdlib.bool
    base.allEqual :: (* -> * -> stdlib.ordering) -> [*] -> stdlib.bool
    base.any :: (* -> stdlib.bool) -> [*] -> stdlib.bool
    base.applyWhen :: stdlib.bool -> (* -> *) -> * -> *
    base.break :: (* -> stdlib.bool) -> [*] -> ([*], [*])
    base.chunk :: stdlib.int -> [*] -> [[*]]
    base.cmptuple3 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*** -> *** -> stdlib.ordering) -> (*, **, ***) -> (*, **, ***) -> stdlib.ordering
    base.cmptuple4 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*** -> *** -> stdlib.ordering) -> (**** -> **** -> stdlib.ordering) -> (*, **, ***, ****) -> (*, **, ***, ****) -> stdlib.ordering
    base.cmptuple5 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*** -> *** -> stdlib.ordering) -> (**** -> **** -> stdlib.ordering) -> (***** -> ***** -> stdlib.ordering) -> (*, **, ***, ****, *****) -> (*, **, ***, ****, *****) -> stdlib.ordering
    base.cmptuple6 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*** -> *** -> stdlib.ordering) -> (**** -> **** -> stdlib.ordering) -> (***** -> ***** -> stdlib.ordering) -> (*6 -> *6 -> stdlib.ordering) -> (*, **, ***, ****, *****, *6) -> (*, **, ***, ****, *****, *6) -> stdlib.ordering
    base.cmptuple7 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*** -> *** -> stdlib.ordering) -> (**** -> **** -> stdlib.ordering) -> (***** -> ***** -> stdlib.ordering) -> (*6 -> *6 -> stdlib.ordering) -> (*7 -> *7 -> stdlib.ordering) -> (*, **, ***, ****, *****, *6, *7) -> (*, **, ***, ****, *****, *6, *7) -> stdlib.ordering
    base.combinations :: stdlib.int -> [*] -> [[*]]
    base.combinationsWithRep :: stdlib.int -> [*] -> [[*]]
    base.comparing :: (** -> ** -> stdlib.ordering) -> (* -> **) -> * -> * -> stdlib.ordering
    base.concatMap :: (* -> [**]) -> [*] -> [**]
    base.count :: (* -> stdlib.bool) -> [*] -> stdlib.int
    base.curry :: ((*, **) -> ***) -> * -> ** -> ***
    base.curry3 :: ((*, **, ***) -> ****) -> * -> ** -> *** -> ****
    base.cycle :: [*] -> [*]
    base.delete :: (* -> * -> stdlib.ordering) -> * -> [*] -> [*]
    base.deleteAt :: stdlib.int -> [*] -> [*]
    base.descending :: (** -> ** -> stdlib.ordering) -> (* -> **) -> * -> * -> stdlib.ordering
    base.digitVal :: stdlib.char -> stdlib.int
    base.dropWhile :: (* -> stdlib.bool) -> [*] -> [*]
    base.dup :: * -> (*, *)
    base.elem :: (* -> * -> stdlib.ordering) -> * -> [*] -> stdlib.bool
    base.elemIndex :: (* -> stdlib.bool) -> [*] -> maybe.maybe stdlib.int
    base.enumerate :: [*] -> [(stdlib.int, *)]
    base.even :: stdlib.int -> stdlib.bool
    base.find :: (* -> stdlib.bool) -> [*] -> maybe.maybe *
    base.gcd :: stdlib.int -> stdlib.int -> stdlib.int
    base.group :: (* -> * -> stdlib.ordering) -> [*] -> [[*]]
    base.groupBy :: (* -> * -> stdlib.bool) -> [*] -> [[*]]
    base.if' :: stdlib.bool -> * -> * -> *
    base.inits :: [*] -> [[*]]
    base.intercalate :: [*] -> [[*]] -> [*]
    base.interleave :: [*] -> [*] -> [*]
    base.intersperse :: * -> [*] -> [*]
    base.isInfixOf :: (* -> * -> stdlib.ordering) -> [*] -> [*] -> stdlib.bool
    base.isLower :: stdlib.char -> stdlib.bool
    base.isPrefixOf :: (* -> * -> stdlib.ordering) -> [*] -> [*] -> stdlib.bool
    base.isSpace :: stdlib.char -> stdlib.bool
    base.isUpper :: stdlib.char -> stdlib.bool
    base.iterate' :: (* -> *) -> * -> [*]
    base.lcm :: stdlib.int -> stdlib.int -> stdlib.int
    base.length :: [*] -> stdlib.int
    base.mapAccumL :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
    base.mapAccumR :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
    base.mapBoth :: (* -> **) -> (*, *) -> (**, **)
    base.mapFst :: (* -> ***) -> (*, **) -> (***, **)
    base.mapSnd :: (** -> ***) -> (*, **) -> (*, ***)
    base.maxBy :: (** -> ** -> stdlib.ordering) -> (* -> **) -> [*] -> *
    base.minBy :: (** -> ** -> stdlib.ordering) -> (* -> **) -> [*] -> *
    base.modifyAt :: stdlib.int -> (* -> *) -> [*] -> [*]
    base.not :: stdlib.bool -> stdlib.bool
    base.nub :: (* -> * -> stdlib.ordering) -> [*] -> [*]
    base.odd :: stdlib.int -> stdlib.bool
    base.on :: (** -> ** -> ***) -> (* -> **) -> * -> * -> ***
    base.padl :: stdlib.int -> [stdlib.char] -> [stdlib.char]
    base.padr :: stdlib.int -> [stdlib.char] -> [stdlib.char]
    base.pair :: * -> ** -> (*, **)
    base.partition :: (* -> stdlib.bool) -> [*] -> ([*], [*])
    base.permutations :: [*] -> [[*]]
    base.permutationsWithRep :: [*] -> [[*]]
    base.replicate :: stdlib.int -> * -> [*]
    base.scanl :: (* -> ** -> *) -> * -> [**] -> [*]
    base.scanr :: (** -> * -> *) -> * -> [**] -> [*]
    base.setAt :: stdlib.int -> * -> [*] -> [*]
    base.setFst :: *** -> (*, **) -> (***, **)
    base.setSnd :: *** -> (*, **) -> (*, ***)
    base.showtuple3 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*** -> [stdlib.char]) -> (*, **, ***) -> [stdlib.char]
    base.showtuple4 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*** -> [stdlib.char]) -> (**** -> [stdlib.char]) -> (*, **, ***, ****) -> [stdlib.char]
    base.showtuple5 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*** -> [stdlib.char]) -> (**** -> [stdlib.char]) -> (***** -> [stdlib.char]) -> (*, **, ***, ****, *****) -> [stdlib.char]
    base.showtuple6 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*** -> [stdlib.char]) -> (**** -> [stdlib.char]) -> (***** -> [stdlib.char]) -> (*6 -> [stdlib.char]) -> (*, **, ***, ****, *****, *6) -> [stdlib.char]
    base.showtuple7 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*** -> [stdlib.char]) -> (**** -> [stdlib.char]) -> (***** -> [stdlib.char]) -> (*6 -> [stdlib.char]) -> (*7 -> [stdlib.char]) -> (*, **, ***, ****, *****, *6, *7) -> [stdlib.char]
    base.shuffle :: [*] -> [*]
    base.signum :: stdlib.int -> stdlib.int
    base.singleton :: * -> [*]
    base.sortBy :: (* -> * -> stdlib.ordering) -> [*] -> [*]
    base.sortOn :: (** -> ** -> stdlib.ordering) -> (* -> **) -> [*] -> [*]
    base.span :: (* -> stdlib.bool) -> [*] -> ([*], [*])
    base.split :: stdlib.char -> [stdlib.char] -> [[stdlib.char]]
    base.split2 :: [*] -> ([*], [*])
    base.splitAt :: stdlib.int -> [*] -> ([*], [*])
    base.splitOneOf :: (* -> * -> stdlib.ordering) -> [*] -> [*] -> [[*]]
    base.splitWhen :: (* -> stdlib.bool) -> [*] -> [[*]]
    base.stripPrefix :: (* -> * -> stdlib.ordering) -> [*] -> [*] -> maybe.maybe ([*])
    base.swapPair :: (*, **) -> (**, *)
    base.tails :: [*] -> [[*]]
    base.takeWhile :: (* -> stdlib.bool) -> [*] -> [*]
    base.toLower :: stdlib.char -> stdlib.char
    base.toUpper :: stdlib.char -> stdlib.char
    base.transpose :: [[*]] -> [[*]]
    base.triple :: * -> ** -> *** -> (*, **, ***)
    base.uncurry :: (* -> ** -> ***) -> (*, **) -> ***
    base.uncurry3 :: (* -> ** -> *** -> ****) -> (*, **, ***) -> ****
    base.unfoldr :: (** -> maybe.maybe ((*, **))) -> ** -> [*]
    base.uninterleave :: [*] -> ([*], [*])
    base.unzip2 :: [(*, **)] -> ([*], [**])
    base.unzip3 :: [(*, **, ***)] -> ([*], [**], [***])
    base.viewL :: [*] -> maybe.maybe ((*, [*]))
    base.viewR :: [*] -> maybe.maybe (([*], *))
    base.withSuffix :: [stdlib.char] -> [stdlib.char] -> [stdlib.char]
    base.withoutSuffix :: [stdlib.char] -> [stdlib.char] -> [stdlib.char]
    base.words :: [stdlib.char] -> [[stdlib.char]]
    base.xor :: stdlib.bool -> stdlib.bool -> stdlib.bool
    base.zipWith :: (* -> ** -> ***) -> [*] -> [**] -> [***]

### bitSet
    bitSet.bitSet :: type
    bitSet.bs_all :: stdlib.int -> bitSet.bitSet
    bitSet.bs_delete :: stdlib.int -> bitSet.bitSet -> bitSet.bitSet
    bitSet.bs_difference :: stdlib.int -> stdlib.int -> stdlib.int
    bitSet.bs_empty :: bitSet.bitSet
    bitSet.bs_first :: bitSet.bitSet -> stdlib.int
    bitSet.bs_fromInt :: stdlib.int -> bitSet.bitSet
    bitSet.bs_fromList :: [stdlib.int] -> bitSet.bitSet
    bitSet.bs_insert :: stdlib.int -> bitSet.bitSet -> bitSet.bitSet
    bitSet.bs_intersect :: bitSet.bitSet -> bitSet.bitSet -> bitSet.bitSet
    bitSet.bs_last :: bitSet.bitSet -> stdlib.int
    bitSet.bs_member :: stdlib.int -> bitSet.bitSet -> stdlib.bool
    bitSet.bs_null :: bitSet.bitSet -> stdlib.bool
    bitSet.bs_singleton :: stdlib.int -> bitSet.bitSet
    bitSet.bs_size :: bitSet.bitSet -> stdlib.int
    bitSet.bs_toInt :: bitSet.bitSet -> stdlib.int
    bitSet.bs_toList :: bitSet.bitSet -> [stdlib.int]
    bitSet.bs_union :: bitSet.bitSet -> bitSet.bitSet -> bitSet.bitSet
    bitSet.cmpbitSet :: bitSet.bitSet -> bitSet.bitSet -> stdlib.ordering
    bitSet.showbitSet :: bitSet.bitSet -> [stdlib.char]

### either
    either.either * ** ::= either.Left * | either.Right **
    either.Left :: * -> either.either * **
    either.Right :: ** -> either.either * **
    either.cmpeither :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> either.either * ** -> either.either * ** -> stdlib.ordering
    either.e_apply :: either.either * (** -> ***) -> either.either * ** -> either.either * ***
    either.e_bind :: either.either * ** -> (** -> either.either * ***) -> either.either * ***
    either.e_fmap :: (** -> ***) -> either.either * ** -> either.either * ***
    either.e_foldM :: (** -> * -> either.either *** **) -> ** -> [*] -> either.either *** **
    either.e_kbind :: (* -> either.either ** ***) -> (*** -> either.either ** ****) -> * -> either.either ** ****
    either.e_liftA2 :: (** -> *** -> ****) -> either.either * ** -> either.either * *** -> either.either * ****
    either.e_mapM :: (* -> either.either ** ***) -> [*] -> either.either ** ([***])
    either.e_pure :: ** -> either.either * **
    either.e_sequence :: [either.either * **] -> either.either * ([**])
    either.eitherf :: (* -> ***) -> (** -> ***) -> either.either * ** -> ***
    either.fromEither :: ** -> either.either * ** -> **
    either.isLeft :: either.either * ** -> stdlib.bool
    either.isRight :: either.either * ** -> stdlib.bool
    either.partitionEithers :: [either.either * **] -> ([*], [**])
    either.showeither :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> either.either * ** -> [stdlib.char]

### io
    io.handle ::= io.Handle builtin.word#
    io.world ::= io.World
    io.io * == io.world -> (*, io.world)
    io.Handle :: builtin.word# -> io.handle
    io.appendFile :: [stdlib.char] -> [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.clock :: io.world -> (stdlib.int, io.world)
    io.cmphandle :: io.handle -> io.handle -> stdlib.ordering
    io.cmpio :: (* -> * -> stdlib.ordering) -> (io.world -> (*, io.world)) -> (io.world -> (*, io.world)) -> stdlib.ordering
    io.cmpworld :: io.world -> io.world -> stdlib.ordering
    io.errStr :: [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.errStrLn :: [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.getArgs :: io.world -> ([[stdlib.char]], io.world)
    io.getChar :: io.world -> (stdlib.char, io.world)
    io.getContents :: io.world -> ([stdlib.char], io.world)
    io.getLine :: io.world -> ([stdlib.char], io.world)
    io.hGetChar :: io.handle -> io.world -> (stdlib.char, io.world)
    io.hGetContents :: io.handle -> io.world -> ([stdlib.char], io.world)
    io.hGetLine :: io.handle -> io.world -> ([stdlib.char], io.world)
    io.hPutChar :: io.handle -> stdlib.char -> io.world -> (builtin.unit, io.world)
    io.hPutStr :: io.handle -> [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.mtimeFile :: [stdlib.char] -> io.world -> (stdlib.int, io.world)
    io.putChar :: stdlib.char -> io.world -> (builtin.unit, io.world)
    io.putStr :: [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.putStrLn :: [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.readFile :: [stdlib.char] -> io.world -> ([stdlib.char], io.world)
    io.readFileStream :: io.handle -> io.world -> ([stdlib.char], io.world)
    io.showhandle :: io.handle -> [stdlib.char]
    io.showio :: (* -> [stdlib.char]) -> (io.world -> (*, io.world)) -> [stdlib.char]
    io.showworld :: io.world -> [stdlib.char]
    io.stderr :: io.handle
    io.stdin :: io.handle
    io.stdout :: io.handle
    io.systemCmd :: [stdlib.char] -> io.world -> (stdlib.int, io.world)
    io.time :: (* -> **) -> * -> io.world -> ((**, stdlib.int), io.world)
    io.unsafePerformIO :: (io.world -> (*, io.world)) -> *
    io.writeFile :: [stdlib.char] -> [stdlib.char] -> io.world -> (builtin.unit, io.world)
    io.writeFileStream :: io.handle -> [stdlib.char] -> io.world -> (builtin.unit, io.world)

### lens
    lens.lens * ** ::= lens.Lens (* -> **) ((** -> **) -> * -> *)
    lens.Lens :: (* -> **) -> ((** -> **) -> * -> *) -> lens.lens * **
    lens.cmplens :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> lens.lens * ** -> lens.lens * ** -> stdlib.ordering
    lens.composeLens :: lens.lens * ** -> lens.lens ** *** -> lens.lens * ***
    lens.lensFst :: lens.lens ((*, **)) *
    lens.lensSnd :: lens.lens ((*, **)) **
    lens.lensTup3_0 :: lens.lens ((*, **, ***)) *
    lens.lensTup3_1 :: lens.lens ((*, **, ***)) **
    lens.lensTup3_2 :: lens.lens ((*, **, ***)) ***
    lens.lensTup4_0 :: lens.lens ((*, **, ***, ****)) *
    lens.lensTup4_1 :: lens.lens ((*, **, ***, ****)) **
    lens.lensTup4_2 :: lens.lens ((*, **, ***, ****)) ***
    lens.lensTup4_3 :: lens.lens ((*, **, ***, ****)) ****
    lens.lensTup5_0 :: lens.lens ((*, **, ***, ****, *****)) *
    lens.lensTup5_1 :: lens.lens ((*, **, ***, ****, *****)) **
    lens.lensTup5_2 :: lens.lens ((*, **, ***, ****, *****)) ***
    lens.lensTup5_3 :: lens.lens ((*, **, ***, ****, *****)) ****
    lens.lensTup5_4 :: lens.lens ((*, **, ***, ****, *****)) *****
    lens.lensTup6_0 :: lens.lens ((*, **, ***, ****, *****, *6)) *
    lens.lensTup6_1 :: lens.lens ((*, **, ***, ****, *****, *6)) **
    lens.lensTup6_2 :: lens.lens ((*, **, ***, ****, *****, *6)) ***
    lens.lensTup6_3 :: lens.lens ((*, **, ***, ****, *****, *6)) ****
    lens.lensTup6_4 :: lens.lens ((*, **, ***, ****, *****, *6)) *****
    lens.lensTup6_5 :: lens.lens ((*, **, ***, ****, *****, *6)) *6
    lens.lensTup7_0 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) *
    lens.lensTup7_1 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) **
    lens.lensTup7_2 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) ***
    lens.lensTup7_3 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) ****
    lens.lensTup7_4 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) *****
    lens.lensTup7_5 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) *6
    lens.lensTup7_6 :: lens.lens ((*, **, ***, ****, *****, *6, *7)) *7
    lens.lensTup8_0 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) *
    lens.lensTup8_1 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) **
    lens.lensTup8_2 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) ***
    lens.lensTup8_3 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) ****
    lens.lensTup8_4 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) *****
    lens.lensTup8_5 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) *6
    lens.lensTup8_6 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) *7
    lens.lensTup8_7 :: lens.lens ((*, **, ***, ****, *****, *6, *7, *8)) *8
    lens.over :: lens.lens * ** -> (** -> **) -> * -> *
    lens.overFst :: (* -> **) -> (*, ***) -> (**, ***)
    lens.overSnd :: (* -> **) -> (***, *) -> (***, **)
    lens.overTup3_0 :: (* -> **) -> (*, ***, ****) -> (**, ***, ****)
    lens.overTup3_1 :: (* -> **) -> (***, *, ****) -> (***, **, ****)
    lens.overTup3_2 :: (* -> **) -> (***, ****, *) -> (***, ****, **)
    lens.overTup4_0 :: (* -> **) -> (*, ***, ****, *****) -> (**, ***, ****, *****)
    lens.overTup4_1 :: (* -> **) -> (***, *, ****, *****) -> (***, **, ****, *****)
    lens.overTup4_2 :: (* -> **) -> (***, ****, *, *****) -> (***, ****, **, *****)
    lens.overTup4_3 :: (* -> **) -> (***, ****, *****, *) -> (***, ****, *****, **)
    lens.overTup5_0 :: (* -> **) -> (*, ***, ****, *****, *6) -> (**, ***, ****, *****, *6)
    lens.overTup5_1 :: (* -> **) -> (***, *, ****, *****, *6) -> (***, **, ****, *****, *6)
    lens.overTup5_2 :: (* -> **) -> (***, ****, *, *****, *6) -> (***, ****, **, *****, *6)
    lens.overTup5_3 :: (* -> **) -> (***, ****, *****, *, *6) -> (***, ****, *****, **, *6)
    lens.overTup5_4 :: (* -> **) -> (***, ****, *****, *6, *) -> (***, ****, *****, *6, **)
    lens.overTup6_0 :: (* -> **) -> (*, ***, ****, *****, *6, *7) -> (**, ***, ****, *****, *6, *7)
    lens.overTup6_1 :: (* -> **) -> (***, *, ****, *****, *6, *7) -> (***, **, ****, *****, *6, *7)
    lens.overTup6_2 :: (* -> **) -> (***, ****, *, *****, *6, *7) -> (***, ****, **, *****, *6, *7)
    lens.overTup6_3 :: (* -> **) -> (***, ****, *****, *, *6, *7) -> (***, ****, *****, **, *6, *7)
    lens.overTup6_4 :: (* -> **) -> (***, ****, *****, *6, *, *7) -> (***, ****, *****, *6, **, *7)
    lens.overTup6_5 :: (* -> **) -> (***, ****, *****, *6, *7, *) -> (***, ****, *****, *6, *7, **)
    lens.overTup7_0 :: (* -> **) -> (*, ***, ****, *****, *6, *7, *8) -> (**, ***, ****, *****, *6, *7, *8)
    lens.overTup7_1 :: (* -> **) -> (***, *, ****, *****, *6, *7, *8) -> (***, **, ****, *****, *6, *7, *8)
    lens.overTup7_2 :: (* -> **) -> (***, ****, *, *****, *6, *7, *8) -> (***, ****, **, *****, *6, *7, *8)
    lens.overTup7_3 :: (* -> **) -> (***, ****, *****, *, *6, *7, *8) -> (***, ****, *****, **, *6, *7, *8)
    lens.overTup7_4 :: (* -> **) -> (***, ****, *****, *6, *, *7, *8) -> (***, ****, *****, *6, **, *7, *8)
    lens.overTup7_5 :: (* -> **) -> (***, ****, *****, *6, *7, *, *8) -> (***, ****, *****, *6, *7, **, *8)
    lens.overTup7_6 :: (* -> **) -> (***, ****, *****, *6, *7, *8, *) -> (***, ****, *****, *6, *7, *8, **)
    lens.overTup8_0 :: (* -> **) -> (*, ***, ****, *****, *6, *7, *8, *9) -> (**, ***, ****, *****, *6, *7, *8, *9)
    lens.overTup8_1 :: (* -> **) -> (***, *, ****, *****, *6, *7, *8, *9) -> (***, **, ****, *****, *6, *7, *8, *9)
    lens.overTup8_2 :: (* -> **) -> (***, ****, *, *****, *6, *7, *8, *9) -> (***, ****, **, *****, *6, *7, *8, *9)
    lens.overTup8_3 :: (* -> **) -> (***, ****, *****, *, *6, *7, *8, *9) -> (***, ****, *****, **, *6, *7, *8, *9)
    lens.overTup8_4 :: (* -> **) -> (***, ****, *****, *6, *, *7, *8, *9) -> (***, ****, *****, *6, **, *7, *8, *9)
    lens.overTup8_5 :: (* -> **) -> (***, ****, *****, *6, *7, *, *8, *9) -> (***, ****, *****, *6, *7, **, *8, *9)
    lens.overTup8_6 :: (* -> **) -> (***, ****, *****, *6, *7, *8, *, *9) -> (***, ****, *****, *6, *7, *8, **, *9)
    lens.overTup8_7 :: (* -> **) -> (***, ****, *****, *6, *7, *8, *9, *) -> (***, ****, *****, *6, *7, *8, *9, **)
    lens.set :: lens.lens * ** -> ** -> * -> *
    lens.showlens :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> lens.lens * ** -> [stdlib.char]
    lens.view :: lens.lens * ** -> * -> **
    lens.viewFst :: (*, **) -> *
    lens.viewSnd :: (*, **) -> **
    lens.viewTup3_0 :: (*, **, ***) -> *
    lens.viewTup3_1 :: (*, **, ***) -> **
    lens.viewTup3_2 :: (*, **, ***) -> ***
    lens.viewTup4_0 :: (*, **, ***, ****) -> *
    lens.viewTup4_1 :: (*, **, ***, ****) -> **
    lens.viewTup4_2 :: (*, **, ***, ****) -> ***
    lens.viewTup4_3 :: (*, **, ***, ****) -> ****
    lens.viewTup5_0 :: (*, **, ***, ****, *****) -> *
    lens.viewTup5_1 :: (*, **, ***, ****, *****) -> **
    lens.viewTup5_2 :: (*, **, ***, ****, *****) -> ***
    lens.viewTup5_3 :: (*, **, ***, ****, *****) -> ****
    lens.viewTup5_4 :: (*, **, ***, ****, *****) -> *****
    lens.viewTup6_0 :: (*, **, ***, ****, *****, *6) -> *
    lens.viewTup6_1 :: (*, **, ***, ****, *****, *6) -> **
    lens.viewTup6_2 :: (*, **, ***, ****, *****, *6) -> ***
    lens.viewTup6_3 :: (*, **, ***, ****, *****, *6) -> ****
    lens.viewTup6_4 :: (*, **, ***, ****, *****, *6) -> *****
    lens.viewTup6_5 :: (*, **, ***, ****, *****, *6) -> *6
    lens.viewTup7_0 :: (*, **, ***, ****, *****, *6, *7) -> *
    lens.viewTup7_1 :: (*, **, ***, ****, *****, *6, *7) -> **
    lens.viewTup7_2 :: (*, **, ***, ****, *****, *6, *7) -> ***
    lens.viewTup7_3 :: (*, **, ***, ****, *****, *6, *7) -> ****
    lens.viewTup7_4 :: (*, **, ***, ****, *****, *6, *7) -> *****
    lens.viewTup7_5 :: (*, **, ***, ****, *****, *6, *7) -> *6
    lens.viewTup7_6 :: (*, **, ***, ****, *****, *6, *7) -> *7
    lens.viewTup8_0 :: (*, **, ***, ****, *****, *6, *7, *8) -> *
    lens.viewTup8_1 :: (*, **, ***, ****, *****, *6, *7, *8) -> **
    lens.viewTup8_2 :: (*, **, ***, ****, *****, *6, *7, *8) -> ***
    lens.viewTup8_3 :: (*, **, ***, ****, *****, *6, *7, *8) -> ****
    lens.viewTup8_4 :: (*, **, ***, ****, *****, *6, *7, *8) -> *****
    lens.viewTup8_5 :: (*, **, ***, ****, *****, *6, *7, *8) -> *6
    lens.viewTup8_6 :: (*, **, ***, ****, *****, *6, *7, *8) -> *7
    lens.viewTup8_7 :: (*, **, ***, ****, *****, *6, *7, *8) -> *8

### map
    map.m_map * ** == avl.avlTree ((*, **))
    map.cmpm_map :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **)) -> stdlib.ordering
    map.m_adjust :: (* -> * -> stdlib.ordering) -> (** -> **) -> * -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_alter :: (* -> * -> stdlib.ordering) -> (maybe.maybe ** -> maybe.maybe **) -> * -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_delete :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_elems :: avl.avlTree ((*, **)) -> [**]
    map.m_filter :: (* -> * -> stdlib.ordering) -> (** -> stdlib.bool) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_filterWithKey :: (* -> * -> stdlib.ordering) -> ((*, **) -> stdlib.bool) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_findWithDefault :: (* -> * -> stdlib.ordering) -> ** -> * -> avl.avlTree ((*, **)) -> **
    map.m_fmap :: (** -> ***) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, ***))
    map.m_fmapWithKey :: (* -> ** -> ***) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, ***))
    map.m_foldl :: (*** -> ** -> ***) -> *** -> avl.avlTree ((*, **)) -> ***
    map.m_foldr :: (** -> *** -> ***) -> *** -> avl.avlTree ((*, **)) -> ***
    map.m_fromList :: (* -> * -> stdlib.ordering) -> [(*, **)] -> avl.avlTree ((*, **))
    map.m_insert :: (* -> * -> stdlib.ordering) -> * -> ** -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_insertWith :: (* -> * -> stdlib.ordering) -> (** -> ** -> **) -> * -> ** -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.m_keys :: avl.avlTree ((*, **)) -> [*]
    map.m_keysSet :: avl.avlTree ((*, **)) -> avl.avlTree *
    map.m_lookup :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree ((*, **)) -> maybe.maybe **
    map.m_mapAccumL :: (**** -> ** -> (****, ***)) -> **** -> avl.avlTree ((*, **)) -> (****, avl.avlTree ((*, ***)))
    map.m_member :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree ((*, **)) -> stdlib.bool
    map.m_singleton :: * -> ** -> avl.avlTree ((*, **))
    map.m_union :: (* -> * -> stdlib.ordering) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **)) -> avl.avlTree ((*, **))
    map.showm_map :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> avl.avlTree ((*, **)) -> [stdlib.char]

### maybe
    maybe.maybe * ::= maybe.Nothing | maybe.Just *
    maybe.Just :: * -> maybe.maybe *
    maybe.Nothing :: maybe.maybe *
    maybe.catMaybes :: [maybe.maybe *] -> [*]
    maybe.cmpmaybe :: (* -> * -> stdlib.ordering) -> maybe.maybe * -> maybe.maybe * -> stdlib.ordering
    maybe.fromJust :: maybe.maybe * -> *
    maybe.fromMaybe :: * -> maybe.maybe * -> *
    maybe.fromMaybef :: ** -> (* -> **) -> maybe.maybe * -> **
    maybe.isJust :: maybe.maybe * -> stdlib.bool
    maybe.isNothing :: maybe.maybe * -> stdlib.bool
    maybe.mapMaybe :: (* -> maybe.maybe **) -> [*] -> [**]
    maybe.mb_alt :: maybe.maybe * -> maybe.maybe * -> maybe.maybe *
    maybe.mb_apply :: maybe.maybe (* -> **) -> maybe.maybe * -> maybe.maybe **
    maybe.mb_bind :: maybe.maybe * -> (* -> maybe.maybe **) -> maybe.maybe **
    maybe.mb_filterM :: (* -> maybe.maybe stdlib.bool) -> [*] -> maybe.maybe ([*])
    maybe.mb_fmap :: (* -> **) -> maybe.maybe * -> maybe.maybe **
    maybe.mb_foldM :: (** -> * -> maybe.maybe **) -> ** -> [*] -> maybe.maybe **
    maybe.mb_kbind :: (* -> maybe.maybe **) -> (** -> maybe.maybe ***) -> * -> maybe.maybe ***
    maybe.mb_left :: maybe.maybe * -> maybe.maybe ** -> maybe.maybe *
    maybe.mb_liftA2 :: (* -> ** -> ***) -> maybe.maybe * -> maybe.maybe ** -> maybe.maybe ***
    maybe.mb_mapM :: (* -> maybe.maybe **) -> [*] -> maybe.maybe ([**])
    maybe.mb_pure :: * -> maybe.maybe *
    maybe.mb_right :: maybe.maybe * -> maybe.maybe ** -> maybe.maybe **
    maybe.mb_sequence :: [maybe.maybe *] -> maybe.maybe ([*])
    maybe.showmaybe :: (* -> [stdlib.char]) -> maybe.maybe * -> [stdlib.char]

### maybeState
    maybeState.maybeState * ** == * -> (maybe.maybe **, *)
    maybeState.cmpmaybeState :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe **, *)) -> stdlib.ordering
    maybeState.mst_alt :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe **, *)) -> * -> (maybe.maybe **, *)
    maybeState.mst_apply :: (* -> (maybe.maybe (** -> ***), *)) -> (* -> (maybe.maybe **, *)) -> * -> (maybe.maybe ***, *)
    maybeState.mst_bind :: (* -> (maybe.maybe **, *)) -> (** -> * -> (maybe.maybe ***, *)) -> * -> (maybe.maybe ***, *)
    maybeState.mst_bind2 :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> (** -> *** -> * -> (maybe.maybe ****, *)) -> * -> (maybe.maybe ****, *)
    maybeState.mst_bind3 :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> (* -> (maybe.maybe ****, *)) -> (** -> *** -> **** -> * -> (maybe.maybe *****, *)) -> * -> (maybe.maybe *****, *)
    maybeState.mst_bind4 :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> (* -> (maybe.maybe ****, *)) -> (* -> (maybe.maybe *****, *)) -> (** -> *** -> **** -> ***** -> * -> (maybe.maybe *6, *)) -> * -> (maybe.maybe *6, *)
    maybeState.mst_fail :: * -> (maybe.maybe **, *)
    maybeState.mst_filterM :: (** -> * -> (maybe.maybe stdlib.bool, *)) -> [**] -> * -> (maybe.maybe ([**]), *)
    maybeState.mst_fmap :: (** -> ***) -> (* -> (maybe.maybe **, *)) -> * -> (maybe.maybe ***, *)
    maybeState.mst_foldM :: (*** -> ** -> * -> (maybe.maybe ***, *)) -> *** -> [**] -> * -> (maybe.maybe ***, *)
    maybeState.mst_forM :: [**] -> (** -> * -> (maybe.maybe ***, *)) -> * -> (maybe.maybe builtin.unit, *)
    maybeState.mst_get :: * -> (maybe.maybe *, *)
    maybeState.mst_join :: (* -> (maybe.maybe (* -> (maybe.maybe **, *)), *)) -> * -> (maybe.maybe **, *)
    maybeState.mst_kbind :: (** -> * -> (maybe.maybe ***, *)) -> (*** -> * -> (maybe.maybe ****, *)) -> ** -> * -> (maybe.maybe ****, *)
    maybeState.mst_left :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> * -> (maybe.maybe **, *)
    maybeState.mst_lift :: (* -> (**, *)) -> * -> (maybe.maybe **, *)
    maybeState.mst_liftA2 :: (** -> *** -> ****) -> (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> * -> (maybe.maybe ****, *)
    maybeState.mst_liftA3 :: (** -> *** -> **** -> *****) -> (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> (* -> (maybe.maybe ****, *)) -> * -> (maybe.maybe *****, *)
    maybeState.mst_liftA4 :: (** -> *** -> **** -> ***** -> *6) -> (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> (* -> (maybe.maybe ****, *)) -> (* -> (maybe.maybe *****, *)) -> * -> (maybe.maybe *6, *)
    maybeState.mst_many :: (* -> (maybe.maybe **, *)) -> * -> (maybe.maybe ([**]), *)
    maybeState.mst_mapM :: (** -> * -> (maybe.maybe ***, *)) -> [**] -> * -> (maybe.maybe ([***]), *)
    maybeState.mst_mapM_ :: (** -> * -> (maybe.maybe ***, *)) -> [**] -> * -> (maybe.maybe builtin.unit, *)
    maybeState.mst_maybe :: maybe.maybe ** -> * -> (maybe.maybe **, *)
    maybeState.mst_modify :: (* -> *) -> * -> (maybe.maybe builtin.unit, *)
    maybeState.mst_pure :: ** -> * -> (maybe.maybe **, *)
    maybeState.mst_put :: * -> * -> (maybe.maybe builtin.unit, *)
    maybeState.mst_right :: (* -> (maybe.maybe **, *)) -> (* -> (maybe.maybe ***, *)) -> * -> (maybe.maybe ***, *)
    maybeState.mst_sequence :: [* -> (maybe.maybe **, *)] -> * -> (maybe.maybe ([**]), *)
    maybeState.mst_sequence_ :: [* -> (maybe.maybe **, *)] -> * -> (maybe.maybe builtin.unit, *)
    maybeState.mst_some :: (* -> (maybe.maybe **, *)) -> * -> (maybe.maybe ([**]), *)
    maybeState.showmaybeState :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (* -> (maybe.maybe **, *)) -> [stdlib.char]

### parser
    parser.parser * == (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.psSt == (builtin.word#, builtin.word#, [stdlib.char])
    parser.cmpparser :: (* -> * -> stdlib.ordering) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> stdlib.ordering
    parser.cmppsSt :: (builtin.word#, builtin.word#, [stdlib.char]) -> (builtin.word#, builtin.word#, [stdlib.char]) -> stdlib.ordering
    parser.p_alt :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_any :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_anyOf :: [stdlib.char] -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_char :: stdlib.char -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_comma :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_cons :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_count :: stdlib.int -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_digit :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_end :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe builtin.unit, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_error :: (builtin.word#, builtin.word#, [stdlib.char]) -> [stdlib.char]
    parser.p_inAngles :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_inBraces :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_inBrackets :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_inParens :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_int :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.int, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_intlist :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([stdlib.int]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_letter :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_many :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_manySepBy :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe **, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([**]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_manyUntil :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe **, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_noneOf :: [stdlib.char] -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_not :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe builtin.unit, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_notChar :: stdlib.char -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_optional :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe (maybe.maybe *), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_peek :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe builtin.unit, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_posint :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.int, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_satisfy :: (stdlib.char -> stdlib.bool) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_skipUntil :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe **, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe **, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_some :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([*]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_someSepBy :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe **, (builtin.word#, builtin.word#, [stdlib.char]))) -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([**]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_space :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe stdlib.char, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_spaces :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([stdlib.char]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_string :: [stdlib.char] -> (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([stdlib.char]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.p_word :: (builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe ([stdlib.char]), (builtin.word#, builtin.word#, [stdlib.char]))
    parser.parse :: ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> [stdlib.char] -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))
    parser.readIntlist :: [stdlib.char] -> [stdlib.int]
    parser.showparser :: (* -> [stdlib.char]) -> ((builtin.word#, builtin.word#, [stdlib.char]) -> (maybe.maybe *, (builtin.word#, builtin.word#, [stdlib.char]))) -> [stdlib.char]
    parser.showpsSt :: (builtin.word#, builtin.word#, [stdlib.char]) -> [stdlib.char]

### set
    set.s_set * == avl.avlTree *
    set.cmps_set :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> stdlib.ordering
    set.s_difference :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> avl.avlTree *
    set.s_filter :: (* -> * -> stdlib.ordering) -> (* -> stdlib.bool) -> avl.avlTree * -> avl.avlTree *
    set.s_fmap :: (** -> ** -> stdlib.ordering) -> (* -> **) -> avl.avlTree * -> avl.avlTree **
    set.s_fromList :: (* -> * -> stdlib.ordering) -> [*] -> avl.avlTree *
    set.s_insert :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> avl.avlTree *
    set.s_insertIfAbsent :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> maybe.maybe (avl.avlTree *)
    set.s_intersect :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> avl.avlTree *
    set.s_lookupGE :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> maybe.maybe *
    set.s_lookupLE :: (* -> * -> stdlib.ordering) -> * -> avl.avlTree * -> maybe.maybe *
    set.s_union :: (* -> * -> stdlib.ordering) -> avl.avlTree * -> avl.avlTree * -> avl.avlTree *
    set.s_viewMax :: avl.avlTree * -> maybe.maybe ((*, avl.avlTree *))
    set.s_viewMin :: avl.avlTree * -> maybe.maybe ((*, avl.avlTree *))
    set.shows_set :: (* -> [stdlib.char]) -> avl.avlTree * -> [stdlib.char]

### state
    state.state * ** == * -> (**, *)
    state.cmpstate :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (* -> (**, *)) -> (* -> (**, *)) -> stdlib.ordering
    state.showstate :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (* -> (**, *)) -> [stdlib.char]
    state.st_apply :: (* -> (** -> ***, *)) -> (* -> (**, *)) -> * -> (***, *)
    state.st_bind :: (* -> (**, *)) -> (** -> * -> (***, *)) -> * -> (***, *)
    state.st_bind2 :: (* -> (**, *)) -> (* -> (***, *)) -> (** -> *** -> * -> (****, *)) -> * -> (****, *)
    state.st_bind3 :: (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (** -> *** -> **** -> * -> (*****, *)) -> * -> (*****, *)
    state.st_bind4 :: (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (* -> (*****, *)) -> (** -> *** -> **** -> ***** -> * -> (*6, *)) -> * -> (*6, *)
    state.st_bind5 :: (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (* -> (*****, *)) -> (* -> (*6, *)) -> (** -> *** -> **** -> ***** -> *6 -> * -> (*7, *)) -> * -> (*7, *)
    state.st_evalState :: (* -> (**, *)) -> * -> **
    state.st_execState :: (* -> (**, *)) -> * -> *
    state.st_filterM :: (** -> * -> (stdlib.bool, *)) -> [**] -> * -> ([**], *)
    state.st_fmap :: (** -> ***) -> (* -> (**, *)) -> * -> (***, *)
    state.st_foldM :: (*** -> ** -> * -> (***, *)) -> *** -> [**] -> * -> (***, *)
    state.st_forM :: [**] -> (** -> * -> (***, *)) -> * -> (builtin.unit, *)
    state.st_get :: * -> (*, *)
    state.st_join :: (* -> (* -> (**, *), *)) -> * -> (**, *)
    state.st_kbind :: (** -> * -> (***, *)) -> (*** -> * -> (****, *)) -> ** -> * -> (****, *)
    state.st_left :: (* -> (**, *)) -> (* -> (***, *)) -> * -> (**, *)
    state.st_liftA2 :: (** -> *** -> ****) -> (* -> (**, *)) -> (* -> (***, *)) -> * -> (****, *)
    state.st_liftA3 :: (** -> *** -> **** -> *****) -> (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> * -> (*****, *)
    state.st_liftA4 :: (** -> *** -> **** -> ***** -> *6) -> (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (* -> (*****, *)) -> * -> (*6, *)
    state.st_liftA5 :: (** -> *** -> **** -> ***** -> *6 -> *7) -> (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (* -> (*****, *)) -> (* -> (*6, *)) -> * -> (*7, *)
    state.st_liftA6 :: (** -> *** -> **** -> ***** -> *6 -> *7 -> *8) -> (* -> (**, *)) -> (* -> (***, *)) -> (* -> (****, *)) -> (* -> (*****, *)) -> (* -> (*6, *)) -> (* -> (*7, *)) -> * -> (*8, *)
    state.st_mapM :: (** -> * -> (***, *)) -> [**] -> * -> ([***], *)
    state.st_mapM_ :: (** -> * -> (***, *)) -> [**] -> * -> (builtin.unit, *)
    state.st_modify :: (* -> *) -> * -> (builtin.unit, *)
    state.st_pure :: ** -> * -> (**, *)
    state.st_put :: * -> * -> (builtin.unit, *)
    state.st_right :: (* -> (**, *)) -> (* -> (***, *)) -> * -> (***, *)
    state.st_runState :: (* -> (**, *)) -> * -> (**, *)
    state.st_sequence :: [* -> (**, *)] -> * -> ([**], *)
    state.st_sequence_ :: [* -> (**, *)] -> * -> (builtin.unit, *)

### stdlib
    stdlib.bool ::= stdlib.False | stdlib.True
    stdlib.char ::= stdlib.C# builtin.word#
    stdlib.int ::= stdlib.I# builtin.word#
    stdlib.ordering ::= stdlib.EQ | stdlib.LT | stdlib.GT
    stdlib.num == stdlib.int
    stdlib.ordI * == * -> * -> stdlib.ordering
    stdlib.showI * == * -> [stdlib.char]
    stdlib.string == [stdlib.char]
    (stdlib.!) :: [*] -> stdlib.int -> *
    (stdlib.#) :: [*] -> stdlib.int
    (stdlib.$) :: (* -> **) -> * -> **
    (stdlib.&) :: stdlib.bool -> stdlib.bool -> stdlib.bool
    (stdlib.*) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib.+) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib.++) :: [*] -> [*] -> [*]
    (stdlib.-) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib..&.) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib..) :: (** -> ***) -> (* -> **) -> * -> ***
    (stdlib..<<.) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib..>) :: (* -> **) -> (** -> ***) -> * -> ***
    (stdlib..>>.) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib..^.) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib..|.) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib./) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib.<$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.<) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.<.) :: stdlib.char -> stdlib.char -> stdlib.bool
    (stdlib.<=$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.<=) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.<=.) :: stdlib.char -> stdlib.char -> stdlib.bool
    (stdlib.==$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.==) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.==.) :: stdlib.char -> stdlib.char -> stdlib.bool
    (stdlib.>$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.>) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.>.) :: stdlib.char -> stdlib.char -> stdlib.bool
    (stdlib.>=$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.>=) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.>=.) :: stdlib.char -> stdlib.char -> stdlib.bool
    (stdlib.\/) :: stdlib.bool -> stdlib.bool -> stdlib.bool
    (stdlib.^) :: stdlib.int -> stdlib.int -> stdlib.int
    (stdlib.|>) :: * -> (* -> **) -> **
    (stdlib.~) :: stdlib.bool -> stdlib.bool
    (stdlib.~=$) :: [stdlib.char] -> [stdlib.char] -> stdlib.bool
    (stdlib.~=) :: stdlib.int -> stdlib.int -> stdlib.bool
    (stdlib.~=.) :: stdlib.char -> stdlib.char -> stdlib.bool
    stdlib.C# :: builtin.word# -> stdlib.char
    stdlib.EQ :: stdlib.ordering
    stdlib.False :: stdlib.bool
    stdlib.GT :: stdlib.ordering
    stdlib.I# :: builtin.word# -> stdlib.int
    stdlib.LT :: stdlib.ordering
    stdlib.True :: stdlib.bool
    stdlib._eq :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib._ge :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib._gt :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib._le :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib._lt :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib._ne :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.bool
    stdlib.abs :: stdlib.int -> stdlib.int
    stdlib.and :: [stdlib.bool] -> stdlib.bool
    stdlib.apply :: (* -> **) -> * -> **
    stdlib.blackHole :: *
    stdlib.caseFail :: ([stdlib.char], builtin.word#, builtin.word#) -> *
    stdlib.cmpFn :: (* -> **) -> (* -> **) -> stdlib.ordering
    stdlib.cmpTags :: * -> * -> stdlib.ordering
    stdlib.cmpbool :: stdlib.bool -> stdlib.bool -> stdlib.ordering
    stdlib.cmpchar :: stdlib.char -> stdlib.char -> stdlib.ordering
    stdlib.cmpint :: stdlib.int -> stdlib.int -> stdlib.ordering
    stdlib.cmplist :: (* -> * -> stdlib.ordering) -> [*] -> [*] -> stdlib.ordering
    stdlib.cmpnum :: stdlib.int -> stdlib.int -> stdlib.ordering
    stdlib.cmpordI :: (* -> * -> stdlib.ordering) -> (* -> * -> stdlib.ordering) -> (* -> * -> stdlib.ordering) -> stdlib.ordering
    stdlib.cmpordering :: stdlib.ordering -> stdlib.ordering -> stdlib.ordering
    stdlib.cmpshowI :: (* -> * -> stdlib.ordering) -> (* -> [stdlib.char]) -> (* -> [stdlib.char]) -> stdlib.ordering
    stdlib.cmpstring :: [stdlib.char] -> [stdlib.char] -> stdlib.ordering
    stdlib.cmptuple2 :: (* -> * -> stdlib.ordering) -> (** -> ** -> stdlib.ordering) -> (*, **) -> (*, **) -> stdlib.ordering
    stdlib.cmpunit :: builtin.unit -> builtin.unit -> stdlib.ordering
    stdlib.cmpword# :: builtin.word# -> builtin.word# -> stdlib.ordering
    stdlib.code :: stdlib.char -> stdlib.int
    stdlib.compare :: (* -> * -> stdlib.ordering) -> * -> * -> stdlib.ordering
    stdlib.complement :: stdlib.int -> stdlib.int
    stdlib.concat :: [[*]] -> [*]
    stdlib.const :: * -> ** -> *
    stdlib.converse :: (* -> ** -> ***) -> ** -> * -> ***
    stdlib.decode :: stdlib.int -> stdlib.char
    stdlib.digit :: stdlib.char -> stdlib.bool
    stdlib.div :: stdlib.int -> stdlib.int -> stdlib.int
    stdlib.divmod :: stdlib.int -> stdlib.int -> (stdlib.int, stdlib.int)
    stdlib.drop :: stdlib.int -> [*] -> [*]
    stdlib.entier :: stdlib.int -> stdlib.int
    stdlib.error :: [stdlib.char] -> *
    stdlib.error# :: [stdlib.char] -> *
    stdlib.exit :: stdlib.int -> *
    stdlib.filter :: (* -> stdlib.bool) -> [*] -> [*]
    stdlib.fix :: (* -> *) -> *
    stdlib.foldl :: (* -> ** -> *) -> * -> [**] -> *
    stdlib.foldl1 :: (* -> * -> *) -> [*] -> *
    stdlib.foldr :: (* -> ** -> **) -> ** -> [*] -> **
    stdlib.foldr1 :: (* -> * -> *) -> [*] -> *
    stdlib.fst :: (*, **) -> *
    stdlib.getTag :: * -> stdlib.int
    stdlib.hd :: [*] -> *
    stdlib.id :: * -> *
    stdlib.index :: [*] -> [stdlib.int]
    stdlib.init :: [*] -> [*]
    stdlib.intval :: [stdlib.char] -> stdlib.int
    stdlib.iterate :: (* -> *) -> * -> [*]
    stdlib.last :: [*] -> *
    stdlib.lay :: [[stdlib.char]] -> [stdlib.char]
    stdlib.letter :: stdlib.char -> stdlib.bool
    stdlib.lines :: [stdlib.char] -> [[stdlib.char]]
    stdlib.map :: (* -> **) -> [*] -> [**]
    stdlib.map2 :: (* -> ** -> ***) -> [*] -> [**] -> [***]
    stdlib.matchFail :: ([stdlib.char], builtin.word#, builtin.word#) -> *
    stdlib.max :: (* -> * -> stdlib.ordering) -> [*] -> *
    stdlib.max2 :: (* -> * -> stdlib.ordering) -> * -> * -> *
    stdlib.member :: (* -> * -> stdlib.ordering) -> [*] -> * -> stdlib.bool
    stdlib.min :: (* -> * -> stdlib.ordering) -> [*] -> *
    stdlib.min2 :: (* -> * -> stdlib.ordering) -> * -> * -> *
    stdlib.mod :: stdlib.int -> stdlib.int -> stdlib.int
    stdlib.neg :: stdlib.int -> stdlib.int
    stdlib.null :: [*] -> stdlib.bool
    stdlib.numval :: [stdlib.char] -> stdlib.int
    stdlib.or :: [stdlib.bool] -> stdlib.bool
    stdlib.product :: [stdlib.int] -> stdlib.int
    stdlib.quot :: stdlib.int -> stdlib.int -> stdlib.int
    stdlib.quotrem :: stdlib.int -> stdlib.int -> (stdlib.int, stdlib.int)
    stdlib.range :: stdlib.int -> stdlib.int -> [stdlib.int]
    stdlib.rangeBy :: stdlib.int -> stdlib.int -> stdlib.int -> [stdlib.int]
    stdlib.rangeByFrom :: stdlib.int -> stdlib.int -> [stdlib.int]
    stdlib.rangeFrom :: stdlib.int -> [stdlib.int]
    stdlib.rapply :: * -> (* -> **) -> **
    stdlib.readByteStream :: builtin.word# -> [stdlib.char]
    stdlib.rem :: stdlib.int -> stdlib.int -> stdlib.int
    stdlib.rep :: stdlib.int -> * -> [*]
    stdlib.repeat :: * -> [*]
    stdlib.reverse :: [*] -> [*]
    stdlib.seq :: * -> ** -> **
    stdlib.show :: (* -> [stdlib.char]) -> * -> [stdlib.char]
    stdlib.showCharEscaped :: stdlib.char -> [stdlib.char]
    stdlib.showCharUnquoted :: stdlib.char -> [stdlib.char]
    stdlib.showFn :: (* -> **) -> [stdlib.char]
    stdlib.showbin :: stdlib.int -> [stdlib.char]
    stdlib.showbool :: stdlib.bool -> [stdlib.char]
    stdlib.showchar :: stdlib.char -> [stdlib.char]
    stdlib.showhex :: stdlib.int -> [stdlib.char]
    stdlib.showint :: stdlib.int -> [stdlib.char]
    stdlib.showintBase :: stdlib.int -> [stdlib.char] -> stdlib.int -> [stdlib.char]
    stdlib.showlist :: (* -> [stdlib.char]) -> [*] -> [stdlib.char]
    stdlib.shownum :: stdlib.int -> [stdlib.char]
    stdlib.showoct :: stdlib.int -> [stdlib.char]
    stdlib.showordI :: (* -> [stdlib.char]) -> (* -> * -> stdlib.ordering) -> [stdlib.char]
    stdlib.showordering :: stdlib.ordering -> [stdlib.char]
    stdlib.showshowI :: (* -> [stdlib.char]) -> (* -> [stdlib.char]) -> [stdlib.char]
    stdlib.showstring :: [stdlib.char] -> [stdlib.char]
    stdlib.showtuple2 :: (* -> [stdlib.char]) -> (** -> [stdlib.char]) -> (*, **) -> [stdlib.char]
    stdlib.showunit :: builtin.unit -> [stdlib.char]
    stdlib.showword# :: builtin.word# -> [stdlib.char]
    stdlib.snd :: (*, **) -> **
    stdlib.subtract :: stdlib.int -> stdlib.int -> stdlib.int
    stdlib.sum :: [stdlib.int] -> stdlib.int
    stdlib.take :: stdlib.int -> [*] -> [*]
    stdlib.thenCmp :: stdlib.ordering -> stdlib.ordering -> stdlib.ordering
    stdlib.tl :: [*] -> [*]
    stdlib.trace :: [stdlib.char] -> * -> *
    stdlib.undef :: *
    stdlib.unlines :: [[stdlib.char]] -> [stdlib.char]
    stdlib.unreachable :: *
    stdlib.writeByteStream :: builtin.word# -> [stdlib.char] -> [stdlib.char]
    stdlib.zip2 :: [*] -> [**] -> [(*, **)]
    stdlib.zip3 :: [*] -> [**] -> [***] -> [(*, **, ***)]

