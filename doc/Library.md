# Admiran Library Modules

This file documents the interface for each of the definitions in the Admiran library modules

### builtin

Built-in values and functions implemented in the compiler or the runtime.  In addition to those
shown here, tuples of any arity are dynamically created and implicitly defined.

    Unit              :: unit                          || parsed as "()" in the parser
    Nil               :: [*]                           || parsed as "[]" in the parser
    :                 :: * -> [*] -> [*]
    
    || primitive arithmetic operations on word#
    cmp#              :: word# -> word# -> word#       || cmp returns 0# -> EQ, 1# -> LT, 2# -> GT
    +#                :: word# -> word# -> word#
    -#                :: word# -> word# -> word#
    *#                :: word# -> word# -> word#
    quotrem#          :: word# -> word# -> (word#, word#)
    divmod#           :: word# -> word# -> (word#, word#)
    
    || primitive bit-wise operations on word#
    band#             :: word# -> word# -> word#
    bor#              :: word# -> word# -> word#
    bxor#             :: word# -> word# -> word#
    bnot#             :: word# -> word#
    bshl#             :: word# -> word# -> word#
    bshr#             :: word# -> word# -> word#
    
    || operations on packed byte streams
    allocByteStream#  :: word# -> word#                || return a ref to a newly-allocated byteStream
    allocFileStream#  :: word# -> word# -> word#       || return a ref to a newly-allocated byteStream with specified fd field
    readByteStream#   :: word# -> word#                || read a byte from byteStream ref
    writeByteStream#  :: word# -> word# -> word#       || write a byte to a byteStream ref, returning success/fail indication
    
    || operations on arrays
    allocArray#       :: word# -> word#                || alloc a contiguous array of N entries (uninitialized), and return its ref
    fillArray#        :: word# -> * -> unit            || fill an existing array with a constant value
    copyArray#        :: word# -> word# -> unit        || copy contents of src array to dst array (already allocated, sizes must match!)
    readArray#        :: word# -> word# -> *           || read a value from the array ref at the given index
    writeArray#       :: word# -> word# -> * -> unit   || write a value to the array ref at the given index
    
    || compiler support
    getTag#           :: * -> word#                    || get tag of a constructor application (for use in ordI instances)
    
    || system operations
    exit#             :: word# -> *                    || exit with the status word (return type is * for typechecker)
    openFileRead#     :: word# -> word#                || open file named by a byteStream for read and return a file buffer byteStream
    openFileWrite#    :: word# -> word#                || open or create file named by a byteStream for write
    openFileAppend#   :: word# -> word#                || open or create file named by a byteStream for append
    closeFile#        :: word# -> unit                 || close an open file buffer byteStream
    readFile#         :: word# -> word#                || fill a file buffer byteStream from the file, and return the read status
    writeFile#        :: word# -> word#                || write a file buffer byteStream to the file, and return the status
    mtimeFile#        :: word# -> word#                || get the modification timestamp for a file (or 0, if non-existent)
    getArg#           :: word# -> word#                || get arg count [0] or argument readStream [1 .. argc -1]
    systemCmd#        :: word# -> word#                || execute the shell command specified by a byteStream and return its int result
    clock#            :: word# -> word#                || return the current user time clock (usec); argument is unused to force function

### astar
astar.am -- implementation of the A-Star search algorithm

    astar.mstate * ** == (**, s.set ((int, *)), m.map * int, m.map * int, m.map * *)
    astar.rstate * ** == (ordI *, ordI ((int, *)), * -> bool, (*, **) -> ([*], **), * -> * -> int, * -> int)
    astar.aStarReachable :: ordI * -> * -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> m.map * *
    astar.aStarSolve :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> ([*], **)
    astar.addMoves :: rstate * ** -> mstate * ** -> * -> [*] -> mstate * **
    astar.backtrace :: rstate * ** -> mstate * ** -> * -> ([*], mstate * **)
    astar.getCost :: ordI * -> * -> m.map * int -> int
    astar.maxCost :: int
    astar.solve :: rstate * ** -> mstate * ** -> ([*], mstate * **)

### avl
avl.am -- AVL tree

    avl.avlTree * ::= avl.AVLLeaf | avl.AVLNode * (avlTree *) (avlTree *) int
    avl.AVLLeaf :: avlTree *
    avl.AVLNode :: * -> avlTree * -> avlTree * -> int -> avlTree *
    avl.balance :: avlTree * -> avlTree *
    avl.balanceL :: avlTree * -> avlTree *
    avl.balanceR :: avlTree * -> avlTree *
    avl.cmpavlTree :: ordI * -> ordI (avlTree *)
    avl.computeHeight :: avlTree * -> avlTree *
    avl.delete :: ordI * -> * -> avlTree * -> avlTree *
    avl.delta :: avlTree * -> int
    avl.empty :: avlTree *
    avl.first :: avlTree * -> *
    avl.fmap :: ordI ** -> (* -> **) -> avlTree * -> avlTree **
    avl.foldl :: (** -> * -> **) -> ** -> avlTree * -> **
    avl.foldr :: (* -> ** -> **) -> ** -> avlTree * -> **
    avl.fromList :: ordI * -> [*] -> avlTree *
    avl.height :: avlTree * -> int
    avl.insert :: ordI * -> * -> avlTree * -> avlTree *
    avl.isSingleton :: avlTree * -> bool
    avl.last :: avlTree * -> *
    avl.member :: ordI * -> * -> avlTree * -> bool
    avl.moveR :: avlTree * -> avlTree * -> avlTree *
    avl.null :: avlTree * -> bool
    avl.rfmap :: ordI ** -> avlTree * -> (* -> **) -> avlTree **
    avl.rotL :: avlTree * -> avlTree *
    avl.rotR :: avlTree * -> avlTree *
    avl.singleton :: * -> avlTree *
    avl.size :: avlTree * -> int
    avl.toList :: avlTree * -> [*]
    avl.union :: ordI * -> avlTree * -> avlTree * -> avlTree *

### bag
bag.am -- implementation of a strict multiset using AVL trees

    bag.bag * == m.map * int
    bag.delete :: ordI * -> * -> bag * -> bag *
    bag.deleteTimes :: ordI * -> * -> int -> bag * -> bag *
    bag.fromCountList :: ordI * -> [(*, int)] -> bag *
    bag.fromList :: ordI * -> [*] -> bag *
    bag.insert :: ordI * -> * -> bag * -> bag *
    bag.insertTimes :: ordI * -> * -> int -> bag * -> bag *
    bag.singleton :: * -> bag *
    bag.union :: ordI * -> bag * -> bag * -> bag *
    bag.withKeys :: ordI * -> [*] -> bag *

`bag` also re-exports the following from `map`:

    empty null size member first last lookup findWithDefault elems keys keysSet toList deleteKey
    adjust alter

### base
base.am -- common extensions to Admiran's standard library

    (base.!>) :: * -> (* -> **) -> **
    (base.!?) :: [*] -> int -> maybe *
    (base.$!) :: (* -> **) -> * -> **
    (base.&&&) :: (* -> **) -> (* -> ***) -> * -> (**, ***)
    (base.***) :: (* -> **) -> (*** -> ****) -> (*, ***) -> (**, ****)
    (base.|*|) :: [*] -> [**] -> [(*, **)]
    base.all :: (* -> bool) -> [*] -> bool
    base.allEqual :: ordI * -> [*] -> bool
    base.any :: (* -> bool) -> [*] -> bool
    base.applyWhen :: bool -> (* -> *) -> * -> *
    base.break :: (* -> bool) -> [*] -> ([*], [*])
    base.chunk :: int -> [*] -> [[*]]
    base.cmptuple3 :: ordI * -> ordI ** -> ordI *** -> ordI ((*, **, ***))
    base.cmptuple4 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ((*, **, ***, ****))
    base.cmptuple5 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI ((*, **, ***, ****, *****))
    base.cmptuple6 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI *6 -> ordI ((*, **, ***, ****, *****, *6))
    base.cmptuple7 :: ordI * -> ordI ** -> ordI *** -> ordI **** -> ordI ***** -> ordI *6 -> ordI *7 -> ordI ((*, **, ***, ****, *****, *6, *7))
    base.combinations :: int -> [*] -> [[*]]
    base.combinationsWithRep :: int -> [*] -> [[*]]
    base.comparing :: ordI ** -> (* -> **) -> ordI *
    base.concatMap :: (* -> [**]) -> [*] -> [**]
    base.count :: (* -> bool) -> [*] -> int
    base.curry :: ((*, **) -> ***) -> * -> ** -> ***
    base.curry3 :: ((*, **, ***) -> ****) -> * -> ** -> *** -> ****
    base.cycle :: [*] -> [*]
    base.delete :: ordI * -> * -> [*] -> [*]
    base.deleteAt :: int -> [*] -> [*]
    base.descending :: ordI ** -> (* -> **) -> ordI *
    base.digitVal :: char -> int
    base.dropWhile :: (* -> bool) -> [*] -> [*]
    base.dup :: * -> (*, *)
    base.elem :: ordI * -> * -> [*] -> bool
    base.elemIndex :: (* -> bool) -> [*] -> maybe int
    base.enumerate :: [*] -> [(int, *)]
    base.even :: int -> bool
    base.find :: (* -> bool) -> [*] -> maybe *
    base.gcd :: int -> int -> int
    base.group :: ordI * -> [*] -> [[*]]
    base.groupBy :: (* -> * -> bool) -> [*] -> [[*]]
    base.if' :: bool -> * -> * -> *
    base.inits :: [*] -> [[*]]
    base.intercalate :: [*] -> [[*]] -> [*]
    base.interleave :: [*] -> [*] -> [*]
    base.intersperse :: * -> [*] -> [*]
    base.isInfixOf :: ordI * -> [*] -> [*] -> bool
    base.isLower :: char -> bool
    base.isPrefixOf :: ordI * -> [*] -> [*] -> bool
    base.isSingleton :: [*] -> bool
    base.isSpace :: char -> bool
    base.isUpper :: char -> bool
    base.iterate' :: (* -> *) -> * -> [*]
    base.lcm :: int -> int -> int
    base.length :: [*] -> int
    base.mapAccumL :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
    base.mapAccumR :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
    base.mapBoth :: (* -> **) -> (*, *) -> (**, **)
    base.mapFst :: (* -> ***) -> (*, **) -> (***, **)
    base.mapSnd :: (** -> ***) -> (*, **) -> (*, ***)
    base.maxBy :: ordI ** -> (* -> **) -> [*] -> *
    base.minBy :: ordI ** -> (* -> **) -> [*] -> *
    base.modifyAt :: int -> (* -> *) -> [*] -> [*]
    base.not :: bool -> bool
    base.nub :: ordI * -> [*] -> [*]
    base.odd :: int -> bool
    base.on :: (** -> ** -> ***) -> (* -> **) -> * -> * -> ***
    base.padL :: int -> string -> string
    base.padR :: int -> string -> string
    base.pair :: * -> ** -> (*, **)
    base.partition :: (* -> bool) -> [*] -> ([*], [*])
    base.permutations :: [*] -> [[*]]
    base.permutationsWithRep :: [*] -> [[*]]
    base.replicate :: int -> * -> [*]
    base.safeHd :: [*] -> maybe *
    base.safeInit :: [*] -> maybe ([*])
    base.safeLast :: [*] -> maybe *
    base.safeTl :: [*] -> maybe ([*])
    base.scanl :: (* -> ** -> *) -> * -> [**] -> [*]
    base.scanr :: (** -> * -> *) -> * -> [**] -> [*]
    base.setAt :: int -> * -> [*] -> [*]
    base.setFst :: *** -> (*, **) -> (***, **)
    base.setSnd :: *** -> (*, **) -> (*, ***)
    base.showtuple3 :: showI * -> showI ** -> showI *** -> showI ((*, **, ***))
    base.showtuple4 :: showI * -> showI ** -> showI *** -> showI **** -> showI ((*, **, ***, ****))
    base.showtuple5 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI ((*, **, ***, ****, *****))
    base.showtuple6 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI *6 -> showI ((*, **, ***, ****, *****, *6))
    base.showtuple7 :: showI * -> showI ** -> showI *** -> showI **** -> showI ***** -> showI *6 -> showI *7 -> showI ((*, **, ***, ****, *****, *6, *7))
    base.shuffle :: [*] -> [*]
    base.signum :: int -> int
    base.singleton :: * -> [*]
    base.sortBy :: ordI * -> [*] -> [*]
    base.sortOn :: ordI ** -> (* -> **) -> [*] -> [*]
    base.span :: (* -> bool) -> [*] -> ([*], [*])
    base.split :: char -> string -> [string]
    base.split2 :: [*] -> ([*], [*])
    base.splitAt :: int -> [*] -> ([*], [*])
    base.splitOneOf :: ordI * -> [*] -> [*] -> [[*]]
    base.splitWhen :: (* -> bool) -> [*] -> [[*]]
    base.stripPrefix :: ordI * -> [*] -> [*] -> maybe ([*])
    base.swapPair :: (*, **) -> (**, *)
    base.tails :: [*] -> [[*]]
    base.takeWhile :: (* -> bool) -> [*] -> [*]
    base.toLower :: char -> char
    base.toUpper :: char -> char
    base.transpose :: [[*]] -> [[*]]
    base.triple :: * -> ** -> *** -> (*, **, ***)
    base.uncurry :: (* -> ** -> ***) -> (*, **) -> ***
    base.uncurry3 :: (* -> ** -> *** -> ****) -> (*, **, ***) -> ****
    base.unfoldr :: (** -> maybe ((*, **))) -> ** -> [*]
    base.uninterleave :: [*] -> ([*], [*])
    base.unzip2 :: [(*, **)] -> ([*], [**])
    base.unzip3 :: [(*, **, ***)] -> ([*], [**], [***])
    base.viewL :: [*] -> maybe ((*, [*]))
    base.viewR :: [*] -> maybe (([*], *))
    base.withSuffix :: string -> string -> string
    base.withoutSuffix :: string -> string -> string
    base.words :: string -> [string]
    base.xor :: bool -> bool -> bool
    base.zipWith :: (* -> ** -> ***) -> [*] -> [**] -> [***]

### bfs
bfs.am -- implementation of a shortest path finder using a breadth-first search

    bfs.backtrace :: ordI * -> * -> ** -> m.map * * -> ([*], **)
    bfs.bfsSolve :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> ([*], **)

### bitSet
bitSet.am -- representation of a set of small (< 64) natural numbers using an int, as an abstract type

    bitSet.all :: int -> bitSet
    bitSet.cmpbitSet :: bitSet -> bitSet -> ordering
    bitSet.delete :: int -> bitSet -> bitSet
    bitSet.difference :: bitSet -> bitSet -> bitSet
    bitSet.empty :: bitSet
    bitSet.first :: bitSet -> int
    bitSet.fromInt :: int -> bitSet
    bitSet.fromList :: [int] -> bitSet
    bitSet.insert :: int -> bitSet -> bitSet
    bitSet.intersect :: bitSet -> bitSet -> bitSet
    bitSet.last :: bitSet -> int
    bitSet.member :: int -> bitSet -> bool
    bitSet.null :: bitSet -> bool
    bitSet.showbitSet :: bitSet -> string
    bitSet.singleton :: int -> bitSet
    bitSet.size :: bitSet -> int
    bitSet.toInt :: bitSet -> int
    bitSet.toList :: bitSet -> [int]
    bitSet.union :: bitSet -> bitSet -> bitSet

### dequeue
dequeue.am -- a double-ended queue of elements that allows quick insertion / deletion at both ends

    dequeue.dequeue * ::= dequeue.FT0 | dequeue.FT1 * | dequeue.FT2 * * | dequeue.FT3 * * * | dequeue.FTN (dequeue *) (dequeue (dequeue *)) (dequeue *)
    dequeue.FT0 :: dequeue *
    dequeue.FT1 :: * -> dequeue *
    dequeue.FT2 :: * -> * -> dequeue *
    dequeue.FT3 :: * -> * -> * -> dequeue *
    dequeue.FTN :: dequeue * -> dequeue (dequeue *) -> dequeue * -> dequeue *
    dequeue.addL :: * -> dequeue * -> dequeue *
    dequeue.addR :: * -> dequeue * -> dequeue *
    dequeue.empty :: dequeue *
    dequeue.fromList :: [*] -> dequeue *
    dequeue.isSat :: dequeue * -> bool
    dequeue.null :: dequeue * -> bool
    dequeue.singleton :: * -> dequeue *
    dequeue.size :: dequeue * -> int
    dequeue.toList :: dequeue * -> [*]
    dequeue.viewL :: dequeue * -> maybe ((*, dequeue *))
    dequeue.viewR :: dequeue * -> maybe ((*, dequeue *))

### either
either.am -- sum type of two distinct types

    either.either * ** ::= either.Left * | either.Right **
    (either.<$>) :: (** -> ***) -> either * ** -> either * ***
    (either.<&>) :: either * ** -> (** -> ***) -> either * ***
    (either.<*>) :: either * (** -> ***) -> either * ** -> either * ***
    (either.<<) :: either * ** -> either * *** -> either * **
    (either.>=>) :: (* -> either ** ***) -> (*** -> either ** ****) -> * -> either ** ****
    (either.>>) :: either * ** -> either * *** -> either * ***
    (either.>>=) :: either * ** -> (** -> either * ***) -> either * ***
    either.Left :: * -> either * **
    either.Right :: ** -> either * **
    either.eitherf :: (* -> ***) -> (** -> ***) -> either * ** -> ***
    either.foldM :: (** -> * -> either *** **) -> ** -> [*] -> either *** **
    either.fromEither :: ** -> either * ** -> **
    either.isLeft :: either * ** -> bool
    either.isRight :: either * ** -> bool
    either.liftA2 :: (** -> *** -> ****) -> either * ** -> either * *** -> either * ****
    either.mapM :: (* -> either ** ***) -> [*] -> either ** ([***])
    either.partitionEithers :: [either * **] -> ([*], [**])
    either.pure :: ** -> either * **
    either.sequence :: [either * **] -> either * ([**])

### fix16
fixed-point representation with 16-bit fractional part, as a substitute for floats

    (fix16.*%) :: fix16 -> fix16 -> fix16
    (fix16.+%) :: fix16 -> fix16 -> fix16
    (fix16.-%) :: fix16 -> fix16 -> fix16
    (fix16./%) :: fix16 -> fix16 -> fix16
    (fix16.<%) :: fix16 -> fix16 -> bool
    (fix16.<=%) :: fix16 -> fix16 -> bool
    (fix16.==%) :: fix16 -> fix16 -> bool
    (fix16.>%) :: fix16 -> fix16 -> bool
    (fix16.>=%) :: fix16 -> fix16 -> bool
    (fix16.~=%) :: fix16 -> fix16 -> bool
    fix16.cmpfix16 :: fix16 -> fix16 -> ordering
    fix16.fix16Frac :: fix16 -> int
    fix16.fix16Int :: fix16 -> int
    fix16.fix16val :: string -> fix16
    fix16.showFracDigits :: int -> fix16 -> (int, string)
    fix16.showfix16 :: fix16 -> string
    fix16.showfix16Digits :: int -> fix16 -> string
    fix16.toFix16Frac :: int -> fix16
    fix16.toFix16Int :: int -> fix16

### heap
heap.am -- tree-based priority queue

    heap.heap * ::= heap.Hempty | heap.Heap int (htree *)
    heap.htree * ::= heap.HTnode int ! * (hforest *)
    heap.hforest * == [htree *]
    heap.HTnode :: int -> * -> hforest * -> htree *
    heap.Heap :: int -> htree * -> heap *
    heap.Hempty :: heap *
    heap.empty :: heap *
    heap.f_viewMin :: ordI * -> hforest * -> maybe ((htree *, hforest *))
    heap.fromList :: ordI * -> [*] -> heap *
    heap.ins :: ordI * -> htree * -> hforest * -> hforest *
    heap.insert :: ordI * -> * -> heap * -> heap *
    heap.link :: ordI * -> htree * -> htree * -> htree *
    heap.null :: heap * -> bool
    heap.rank :: htree * -> int
    heap.root :: htree * -> *
    heap.singleton :: * -> heap *
    heap.size :: heap * -> int
    heap.skewInsert :: ordI * -> htree * -> hforest * -> hforest *
    heap.skewLink :: ordI * -> htree * -> htree * -> htree * -> htree *
    heap.skewMeld :: ordI * -> hforest * -> hforest * -> hforest *
    heap.splitForest :: int -> hforest * -> hforest * -> hforest * -> (hforest *, hforest *, hforest *)
    heap.t_toList :: htree * -> [*]
    heap.toList :: heap * -> [*]
    heap.union :: ordI * -> heap * -> heap * -> heap *
    heap.unionUniq :: ordI * -> hforest * -> hforest * -> hforest *
    heap.uniqify :: ordI * -> hforest * -> hforest *
    heap.viewMin :: ordI * -> heap * -> maybe ((*, heap *))

### io
io.am -- the IO monad for sequencing access to the outside world, based upon the state monad

    io.handle ::= io.Handle word#
    io.world ::= io.World
    io.io * == state world *
    io.Handle :: word# -> handle
    io.World :: world
    io.appendFile :: string -> string -> io builtin.unit
    io.clock :: io int
    io.errStr :: string -> io builtin.unit
    io.errStrLn :: string -> io builtin.unit
    io.getArgs :: io ([string])
    io.getChar :: io char
    io.getContents :: io string
    io.getLine :: io string
    io.hGetChar :: handle -> io char
    io.hGetContents :: handle -> io string
    io.hGetLine :: handle -> io string
    io.hPutChar :: handle -> char -> io builtin.unit
    io.hPutStr :: handle -> string -> io builtin.unit
    io.mtimeFile :: string -> io int
    io.putChar :: char -> io builtin.unit
    io.putStr :: string -> io builtin.unit
    io.putStrLn :: string -> io builtin.unit
    io.readFile :: string -> io string
    io.readFile' :: string -> io string
    io.readFileStream :: handle -> io string
    io.stderr :: handle
    io.stdin :: handle
    io.stdout :: handle
    io.systemCmd :: string -> io int
    io.time :: (* -> **) -> * -> io ((**, int))
    io.unsafePerformIO :: io * -> *
    io.writeFile :: string -> string -> io builtin.unit
    io.writeFileStream :: handle -> string -> io builtin.unit

`io` also re-exports the following from `state`:

    pure (>>=) (>=>) mapM mapM_ forM foldM (<$>) (<&>) (<*>) liftA2 liftA3 (<<) (>>) bind2 bind3
    sequence sequence_

### ioStream
ioStream.am -- io with streams

    ioStream.streamFile :: string -> io (stream char word#)

### lens
lens.am -- optics for accessing nested structures

    lens.lens * ** ::= lens.Lens (* -> **) ((** -> **) -> * -> *)
    lens.Lens :: (* -> **) -> ((** -> **) -> * -> *) -> lens * **
    lens.composeLens :: lens * ** -> lens ** *** -> lens * ***
    lens.over :: lens * ** -> (** -> **) -> * -> *
    lens.set :: lens * ** -> ** -> * -> *
    lens.view :: lens * ** -> * -> **

In addition to the explicit data type and functions shown here, lenses for tuples 2 - 8
are implemented, with explicit view and over functions, as well as the lenses, to allow optimization of code that performs
explicit view/over operations on known tuples.  Also, by having the view/over functions defined at the top-level
of the module, inlining Lens operations have a better opportunity to inline the view or over fn as well, even
when composing lenses.  The names of these are specified as `lensTup<arity>_<field>` where `<arity>` is the tuple arity
and `<field>` is the index of the tuple field being specified, e.g.

    lensTup3_0          || the lens data structure for the first field in a 3-tuple
    viewTup4_1          || the function to view the second field in a 4-tuple

### map
map.am -- implementation of a strict map from key to value, using AVL trees

    map.map * ** == avlTree ((*, **))
    (map.<$>) :: (** -> ***) -> map * ** -> map * ***
    (map.<&>) :: map * ** -> (** -> ***) -> map * ***
    map.adjust :: ordI * -> (** -> **) -> * -> map * ** -> map * **
    map.alter :: ordI * -> (maybe ** -> maybe **) -> * -> map * ** -> map * **
    map.delete :: ordI * -> * -> map * ** -> map * **
    map.elems :: map * ** -> [**]
    map.filter :: ordI * -> (** -> bool) -> map * ** -> map * **
    map.filterWithKey :: ordI * -> ((*, **) -> bool) -> map * ** -> map * **
    map.findWithDefault :: ordI * -> ** -> * -> map * ** -> **
    map.fmapWithKey :: (* -> ** -> ***) -> map * ** -> map * ***
    map.foldl :: (*** -> ** -> ***) -> *** -> map * ** -> ***
    map.foldr :: (** -> *** -> ***) -> *** -> map * ** -> ***
    map.fromList :: ordI * -> [(*, **)] -> map * **
    map.insert :: ordI * -> * -> ** -> map * ** -> map * **
    map.insertWith :: ordI * -> (** -> ** -> **) -> * -> ** -> map * ** -> map * **
    map.keys :: map * ** -> [*]
    map.keysSet :: map * ** -> avlTree *
    map.lookup :: ordI * -> * -> map * ** -> maybe **
    map.mapAccumL :: (**** -> ** -> (****, ***)) -> **** -> map * ** -> (****, map * ***)
    map.member :: ordI * -> * -> map * ** -> bool
    map.singleton :: * -> ** -> map * **
    map.union :: ordI * -> map * ** -> map * ** -> map * **

`map` also re-exports the following from `avl`:

    empty null isSingleton size first last toList



### maybe
maybe.am -- sum type of "Nothing" and another type

    maybe.maybe * ::= maybe.Nothing | maybe.Just *
    (maybe.<$>) :: (* -> **) -> maybe * -> maybe **
    (maybe.<&>) :: maybe * -> (* -> **) -> maybe **
    (maybe.<*>) :: maybe (* -> **) -> maybe * -> maybe **
    (maybe.<<) :: maybe * -> maybe ** -> maybe *
    (maybe.<|>) :: maybe * -> maybe * -> maybe *
    (maybe.>=>) :: (* -> maybe **) -> (** -> maybe ***) -> * -> maybe ***
    (maybe.>>) :: maybe * -> maybe ** -> maybe **
    (maybe.>>=) :: maybe * -> (* -> maybe **) -> maybe **
    maybe.Just :: * -> maybe *
    maybe.Nothing :: maybe *
    maybe.catMaybes :: [maybe *] -> [*]
    maybe.filterM :: (* -> maybe bool) -> [*] -> maybe ([*])
    maybe.foldM :: (** -> * -> maybe **) -> ** -> [*] -> maybe **
    maybe.fromJust :: maybe * -> *
    maybe.fromMaybe :: * -> maybe * -> *
    maybe.fromMaybef :: ** -> (* -> **) -> maybe * -> **
    maybe.isJust :: maybe * -> bool
    maybe.isNothing :: maybe * -> bool
    maybe.liftA2 :: (* -> ** -> ***) -> maybe * -> maybe ** -> maybe ***
    maybe.mapM :: (* -> maybe **) -> [*] -> maybe ([**])
    maybe.mapMaybe :: (* -> maybe **) -> [*] -> [**]
    maybe.pure :: * -> maybe *
    maybe.sequence :: [maybe *] -> maybe ([*])

### maybeState
maybeState.am -- functor / applicative / monad / alternative for a state monad augmented with maybe

    maybeState.maybeState * ** == state * (maybe **)
    (maybeState.<$>) :: (** -> ***) -> maybeState * ** -> maybeState * ***
    (maybeState.<&>) :: maybeState * ** -> (** -> ***) -> maybeState * ***
    (maybeState.<*>) :: maybeState * (** -> ***) -> maybeState * ** -> maybeState * ***
    (maybeState.<<) :: maybeState * ** -> maybeState * *** -> maybeState * **
    (maybeState.<|>) :: maybeState * ** -> maybeState * ** -> maybeState * **
    (maybeState.>=>) :: (** -> maybeState * ***) -> (*** -> maybeState * ****) -> ** -> maybeState * ****
    (maybeState.>>) :: maybeState * ** -> maybeState * *** -> maybeState * ***
    (maybeState.>>=) :: maybeState * ** -> (** -> maybeState * ***) -> maybeState * ***
    maybeState.bind2 :: maybeState * ** -> maybeState * *** -> (** -> *** -> maybeState * ****) -> maybeState * ****
    maybeState.bind3 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> (** -> *** -> **** -> maybeState * *****) -> maybeState * *****
    maybeState.bind4 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> (** -> *** -> **** -> ***** -> maybeState * *6) -> maybeState * *6
    maybeState.fail :: maybeState * **
    maybeState.filterM :: (** -> maybeState * bool) -> [**] -> maybeState * ([**])
    maybeState.foldM :: (*** -> ** -> maybeState * ***) -> *** -> [**] -> maybeState * ***
    maybeState.forM :: [**] -> (** -> maybeState * ***) -> maybeState * builtin.unit
    maybeState.get :: maybeState * *
    maybeState.join :: maybeState * (maybeState * **) -> maybeState * **
    maybeState.liftA2 :: (** -> *** -> ****) -> maybeState * ** -> maybeState * *** -> maybeState * ****
    maybeState.liftA3 :: (** -> *** -> **** -> *****) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * *****
    maybeState.liftA4 :: (** -> *** -> **** -> ***** -> *6) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> maybeState * *6
    maybeState.liftMaybe :: maybe ** -> maybeState * **
    maybeState.liftState :: state * ** -> maybeState * **
    maybeState.many :: maybeState * ** -> maybeState * ([**])
    maybeState.mapM :: (** -> maybeState * ***) -> [**] -> maybeState * ([***])
    maybeState.mapM_ :: (** -> maybeState * ***) -> [**] -> maybeState * builtin.unit
    maybeState.modify :: (* -> *) -> maybeState * builtin.unit
    maybeState.over :: lens * ** -> (** -> **) -> maybeState * builtin.unit
    maybeState.pure :: ** -> maybeState * **
    maybeState.put :: * -> maybeState * builtin.unit
    maybeState.sequence :: [maybeState * **] -> maybeState * ([**])
    maybeState.sequence_ :: [maybeState * **] -> maybeState * builtin.unit
    maybeState.set :: lens * ** -> ** -> maybeState * builtin.unit
    maybeState.some :: maybeState * ** -> maybeState * ([**])
    maybeState.view :: lens * ** -> maybeState * **

`maybeState` also re-exports the following from `state`:

    runState evalState execState`

### md5
md5.m -- MD5 Hash Algorithm

    md5.md5State ::= md5.MD5 int int int int
    md5.buffer == vector int
    (md5.!?) :: vector * -> int -> *
    md5.MD5 :: int -> int -> int -> int -> md5State
    md5.addMsgLen :: int -> buffer -> buffer
    md5.emptyBuffer :: buffer
    md5.makeBuffer :: string -> buffer
    md5.md5Add :: md5State -> md5State -> md5State
    md5.md5Hash :: string -> md5State
    md5.md5Hex :: md5State -> string
    md5.md5InitState :: md5State
    md5.md5Rotate :: md5State -> int -> int -> int -> md5State
    md5.md5Round :: md5State -> buffer -> md5State

### memo
memo.am -- memoization of a single-argument function with a state map

    memo.memoSt * ** == state ((ordI *, m.map * **)) **
    memo.memo :: (* -> memoSt * **) -> * -> memoSt * **

### memoTrie
memoTrie.am -- memoization of functions using a lazy mapping of indices in a trie

    memoTrie.tree * ::= memoTrie.Tree (tree *) * (tree *)
    (memoTrie.<$>) :: (* -> **) -> tree * -> tree **
    memoTrie.Tree :: tree * -> * -> tree * -> tree *
    memoTrie.memo :: (* -> int) -> (int -> *) -> (* -> **) -> * -> **
    memoTrie.memochar :: (char -> *) -> char -> *
    memoTrie.memofix :: ((* -> **) -> * -> **) -> ((* -> **) -> * -> **) -> * -> **
    memoTrie.memoint :: (int -> *) -> int -> *
    memoTrie.memolist :: ((* -> [*] -> **) -> * -> [*] -> **) -> ([*] -> **) -> [*] -> **
    memoTrie.memopair :: ((* -> ** -> ***) -> * -> ** -> ***) -> ((** -> ***) -> ** -> ***) -> ((*, **) -> ***) -> (*, **) -> ***
    memoTrie.memostring :: (string -> *) -> string -> *

### parser
parser.am -- a parser combinator library for strings, based upon the maybeState monad, which tracks line and column position for error reporting

    parser.parser * == maybeState psSt *
    parser.psSt == (word#, word#, [std.char])
    (parser.<|>) :: parser * -> parser * -> parser *
    parser.any :: parser std.char
    parser.anyOf :: [std.char] -> parser std.char
    parser.char :: std.char -> parser std.char
    parser.comma :: parser std.char
    parser.cons :: parser * -> parser ([*]) -> parser ([*])
    parser.count :: std.int -> parser * -> parser ([*])
    parser.digit :: parser std.char
    parser.end :: parser builtin.unit
    parser.error :: psSt -> [std.char]
    parser.inAngles :: parser * -> parser *
    parser.inBraces :: parser * -> parser *
    parser.inBrackets :: parser * -> parser *
    parser.inParens :: parser * -> parser *
    parser.int :: parser std.int
    parser.intlist :: parser ([std.int])
    parser.letter :: parser std.char
    parser.many :: parser * -> parser ([*])
    parser.manySepBy :: parser * -> parser ** -> parser ([**])
    parser.manyUntil :: parser * -> parser ** -> parser ([*])
    parser.noneOf :: [std.char] -> parser std.char
    parser.not :: parser * -> parser builtin.unit
    parser.notChar :: std.char -> parser std.char
    parser.optional :: parser * -> parser (maybe *)
    parser.parse :: parser * -> [std.char] -> (maybe *, psSt)
    parser.peek :: parser * -> parser builtin.unit
    parser.posint :: parser std.int
    parser.readIntlist :: std.string -> [std.int]
    parser.satisfy :: (std.char -> bool) -> parser std.char
    parser.skipUntil :: parser * -> parser ** -> parser **
    parser.some :: parser * -> parser ([*])
    parser.someSepBy :: parser * -> parser ** -> parser ([**])
    parser.space :: parser std.char
    parser.spaces :: parser ([std.char])
    parser.string :: [std.char] -> parser ([std.char])
    parser.word :: parser ([std.char])

`parser` also re-exports the following from maybeState

    get put modify pure fail (>>=) (<$>) (<&>) (<*>) (<<) (>>) liftA2 liftA3 liftA4 mapM foldM bind2 bind3

### rws
rws.am -- reader+writer+state functor/applicative/monad with strict writer and state

    rws.rws * ** *** **** == * -> *** -> [**] -> (****, ***, [**])
    (rws.<$>) :: (**** -> *****) -> rws * ** *** **** -> rws * ** *** *****
    (rws.<&>) :: rws * ** *** **** -> (**** -> *****) -> rws * ** *** *****
    (rws.<*>) :: rws * ** *** (**** -> *****) -> rws * ** *** **** -> rws * ** *** *****
    (rws.<<) :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** ****
    (rws.>=>) :: (**** -> rws * ** *** *****) -> (***** -> rws * ** *** *6) -> **** -> rws * ** *** *6
    (rws.>>) :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *****
    (rws.>>=) :: rws * ** *** **** -> (**** -> rws * ** *** *****) -> rws * ** *** *****
    rws.ask :: rws * ** *** *
    rws.asks :: (* -> ****) -> rws * ** *** ****
    rws.bind2 :: rws * ** *** **** -> rws * ** *** ***** -> (**** -> ***** -> rws * ** *** *6) -> rws * ** *** *6
    rws.bind3 :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> (**** -> ***** -> *6 -> rws * ** *** *7) -> rws * ** *** *7
    rws.evalRWS :: rws * ** *** **** -> * -> *** -> [**] -> (****, [**])
    rws.execRWS :: rws * ** *** **** -> * -> *** -> [**] -> (***, [**])
    rws.filterM :: (**** -> rws * ** *** bool) -> [****] -> rws * ** *** ([****])
    rws.foldM :: (***** -> **** -> rws * ** *** *****) -> ***** -> [****] -> rws * ** *** *****
    rws.forM :: [****] -> (**** -> rws * ** *** *****) -> rws * ** *** builtin.unit
    rws.get :: rws * ** *** ***
    rws.join :: rws * ** *** (rws * ** *** ****) -> rws * ** *** ****
    rws.liftA2 :: (**** -> ***** -> *6) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6
    rws.liftA3 :: (**** -> ***** -> *6 -> *7) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> rws * ** *** *7
    rws.liftA4 :: (**** -> ***** -> *6 -> *7 -> *8) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> rws * ** *** *7 -> rws * ** *** *8
    rws.local :: (* -> *) -> rws * ** *** **** -> rws * ** *** ****
    rws.mapM :: (**** -> rws * ** *** *****) -> [****] -> rws * ** *** ([*****])
    rws.mapM_ :: (**** -> rws * ** *** *****) -> [****] -> rws * ** *** builtin.unit
    rws.modify :: (*** -> ***) -> rws * ** *** builtin.unit
    rws.over :: lens *** **** -> (**** -> ****) -> rws * ** *** builtin.unit
    rws.pure :: **** -> rws * ** *** ****
    rws.put :: *** -> rws * ** *** builtin.unit
    rws.runRWS :: rws * ** *** **** -> * -> *** -> [**] -> (****, ***, [**])
    rws.sequence :: [rws * ** *** ****] -> rws * ** *** ([****])
    rws.set :: lens *** **** -> **** -> rws * ** *** builtin.unit
    rws.tell :: ** -> rws * ** *** builtin.unit
    rws.tells :: ([**] -> [**]) -> rws * ** *** builtin.unit
    rws.view :: lens *** **** -> rws * ** *** ****

### set
set.am -- implementation of a strict set using AVL trees

    set.set * == avlTree *
    (set.<$>) :: ordI ** -> (* -> **) -> set * -> set **
    (set.<&>) :: ordI ** -> set * -> (* -> **) -> set **
    set.difference :: ordI * -> set * -> set * -> set *
    set.filter :: ordI * -> (* -> bool) -> set * -> set *
    set.fromList :: ordI * -> [*] -> set *
    set.insert :: ordI * -> * -> set * -> set *
    set.insertIfAbsent :: ordI * -> * -> set * -> maybe (set *)
    set.intersect :: ordI * -> set * -> set * -> set *
    set.lookupGE :: ordI * -> * -> set * -> maybe *
    set.lookupLE :: ordI * -> * -> set * -> maybe *
    set.union :: ordI * -> set * -> set * -> set *
    set.viewMax :: set * -> maybe ((*, set *))
    set.viewMin :: set * -> maybe ((*, set *))

`set` also re-exports the following from `avl`:

    empty null singleton isSingleton size member first last delete toList foldr foldl

### state
state.am -- strict state functor / applicative / monad

    state.state * ** == * -> (**, *)
    (state.<$>) :: (** -> ***) -> state * ** -> state * ***
    (state.<&>) :: state * ** -> (** -> ***) -> state * ***
    (state.<*>) :: state * (** -> ***) -> state * ** -> state * ***
    (state.<<) :: state * ** -> state * *** -> state * **
    (state.>=>) :: (** -> state * ***) -> (*** -> state * ****) -> ** -> state * ****
    (state.>>) :: state * ** -> state * *** -> state * ***
    (state.>>=) :: state * ** -> (** -> state * ***) -> state * ***
    state.bind2 :: state * ** -> state * *** -> (** -> *** -> state * ****) -> state * ****
    state.bind3 :: state * ** -> state * *** -> state * **** -> (** -> *** -> **** -> state * *****) -> state * *****
    state.bind4 :: state * ** -> state * *** -> state * **** -> state * ***** -> (** -> *** -> **** -> ***** -> state * *6) -> state * *6
    state.bind5 :: state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> (** -> *** -> **** -> ***** -> *6 -> state * *7) -> state * *7
    state.evalState :: state * ** -> * -> **
    state.execState :: state * ** -> * -> *
    state.filterM :: (** -> state * bool) -> [**] -> state * ([**])
    state.foldM :: (*** -> ** -> state * ***) -> *** -> [**] -> state * ***
    state.forM :: [**] -> (** -> state * ***) -> state * builtin.unit
    state.get :: state * *
    state.join :: state * (state * **) -> state * **
    state.liftA2 :: (** -> *** -> ****) -> state * ** -> state * *** -> state * ****
    state.liftA3 :: (** -> *** -> **** -> *****) -> state * ** -> state * *** -> state * **** -> state * *****
    state.liftA4 :: (** -> *** -> **** -> ***** -> *6) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6
    state.liftA5 :: (** -> *** -> **** -> ***** -> *6 -> *7) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> state * *7
    state.liftA6 :: (** -> *** -> **** -> ***** -> *6 -> *7 -> *8) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> state * *7 -> state * *8
    state.mapM :: (** -> state * ***) -> [**] -> state * ([***])
    state.mapM_ :: (** -> state * ***) -> [**] -> state * builtin.unit
    state.modify :: (* -> *) -> state * builtin.unit
    state.over :: lens * ** -> (** -> **) -> state * builtin.unit
    state.pure :: ** -> state * **
    state.put :: * -> state * builtin.unit
    state.runState :: state * ** -> * -> (**, *)
    state.sequence :: [state * **] -> state * ([**])
    state.sequence_ :: [state * **] -> state * builtin.unit
    state.set :: lens * ** -> ** -> state * builtin.unit
    state.view :: lens * ** -> state * **

### stdlib
stdlib.am -- standard environment

    stdlib.bool ::= stdlib.False | stdlib.True
    stdlib.char ::= stdlib.C# word#
    stdlib.int ::= stdlib.I# word#
    stdlib.ordering ::= stdlib.EQ | stdlib.LT | stdlib.GT
    stdlib.num == int
    stdlib.ordI * == * -> * -> ordering
    stdlib.showI * == * -> string
    stdlib.string == [char]
    (stdlib.!) :: [*] -> int -> *
    (stdlib.#) :: [*] -> int
    (stdlib.$) :: (* -> **) -> * -> **
    (stdlib.&) :: bool -> bool -> bool
    (stdlib.*) :: int -> int -> int
    (stdlib.+) :: int -> int -> int
    (stdlib.++) :: [*] -> [*] -> [*]
    (stdlib.-) :: int -> int -> int
    (stdlib..&.) :: int -> int -> int
    (stdlib..) :: (** -> ***) -> (* -> **) -> * -> ***
    (stdlib..<<.) :: int -> int -> int
    (stdlib..>) :: (* -> **) -> (** -> ***) -> * -> ***
    (stdlib..>>.) :: int -> int -> int
    (stdlib..^.) :: int -> int -> int
    (stdlib..|.) :: int -> int -> int
    (stdlib./) :: num -> num -> num
    (stdlib.<) :: int -> int -> bool
    (stdlib.<=) :: int -> int -> bool
    (stdlib.==) :: int -> int -> bool
    (stdlib.>) :: int -> int -> bool
    (stdlib.>=) :: int -> int -> bool
    (stdlib.\/) :: bool -> bool -> bool
    (stdlib.^) :: int -> int -> int
    (stdlib.|>) :: * -> (* -> **) -> **
    (stdlib.~) :: bool -> bool
    (stdlib.~=) :: int -> int -> bool
    stdlib.C# :: word# -> char
    stdlib.EQ :: ordering
    stdlib.False :: bool
    stdlib.GT :: ordering
    stdlib.I# :: word# -> int
    stdlib.LT :: ordering
    stdlib.True :: bool
    stdlib._eq :: ordI * -> * -> * -> bool
    stdlib._ge :: ordI * -> * -> * -> bool
    stdlib._gt :: ordI * -> * -> * -> bool
    stdlib._le :: ordI * -> * -> * -> bool
    stdlib._lt :: ordI * -> * -> * -> bool
    stdlib._ne :: ordI * -> * -> * -> bool
    stdlib.abs :: int -> int
    stdlib.and :: [bool] -> bool
    stdlib.apply :: (* -> **) -> * -> **
    stdlib.blackHole :: *
    stdlib.caseFail :: (string, word#, word#) -> *
    stdlib.cmpFn :: ordI (* -> **)
    stdlib.cmpTags :: * -> * -> ordering
    stdlib.cmplist :: ordI * -> ordI ([*])
    stdlib.cmptuple2 :: ordI * -> ordI ** -> ordI ((*, **))
    stdlib.cmpunit :: ordI builtin.unit
    stdlib.cmpword# :: ordI word#
    stdlib.code :: char -> int
    stdlib.compare :: ordI * -> * -> * -> ordering
    stdlib.complement :: int -> int
    stdlib.concat :: [[*]] -> [*]
    stdlib.const :: * -> ** -> *
    stdlib.converse :: (* -> ** -> ***) -> ** -> * -> ***
    stdlib.decode :: int -> char
    stdlib.digit :: char -> bool
    stdlib.div :: int -> int -> int
    stdlib.divmod :: int -> int -> (int, int)
    stdlib.drop :: int -> [*] -> [*]
    stdlib.entier :: num -> num
    stdlib.error :: string -> *
    stdlib.error# :: string -> *
    stdlib.exit :: int -> *
    stdlib.filter :: (* -> bool) -> [*] -> [*]
    stdlib.fix :: (* -> *) -> *
    stdlib.foldl :: (* -> ** -> *) -> * -> [**] -> *
    stdlib.foldl1 :: (* -> * -> *) -> [*] -> *
    stdlib.foldr :: (* -> ** -> **) -> ** -> [*] -> **
    stdlib.foldr1 :: (* -> * -> *) -> [*] -> *
    stdlib.fst :: (*, **) -> *
    stdlib.getTag :: * -> int
    stdlib.hd :: [*] -> *
    stdlib.id :: * -> *
    stdlib.index :: [*] -> [int]
    stdlib.init :: [*] -> [*]
    stdlib.intval :: string -> int
    stdlib.iterate :: (* -> *) -> * -> [*]
    stdlib.last :: [*] -> *
    stdlib.lay :: [string] -> string
    stdlib.letter :: char -> bool
    stdlib.lines :: string -> [string]
    stdlib.map :: (* -> **) -> [*] -> [**]
    stdlib.map2 :: (* -> ** -> ***) -> [*] -> [**] -> [***]
    stdlib.matchFail :: (string, word#, word#) -> *
    stdlib.max :: ordI * -> [*] -> *
    stdlib.max2 :: ordI * -> * -> * -> *
    stdlib.member :: ordI * -> [*] -> * -> bool
    stdlib.min :: ordI * -> [*] -> *
    stdlib.min2 :: ordI * -> * -> * -> *
    stdlib.mod :: int -> int -> int
    stdlib.neg :: int -> int
    stdlib.null :: [*] -> bool
    stdlib.numval :: string -> num
    stdlib.or :: [bool] -> bool
    stdlib.product :: [int] -> int
    stdlib.quot :: int -> int -> int
    stdlib.quotrem :: int -> int -> (int, int)
    stdlib.range :: int -> int -> [int]
    stdlib.rangeBy :: int -> int -> int -> [int]
    stdlib.rangeByFrom :: int -> int -> [int]
    stdlib.rangeFrom :: int -> [int]
    stdlib.rapply :: * -> (* -> **) -> **
    stdlib.readByteStream :: word# -> string
    stdlib.rem :: int -> int -> int
    stdlib.rep :: int -> * -> [*]
    stdlib.repeat :: * -> [*]
    stdlib.reverse :: [*] -> [*]
    stdlib.seq :: * -> ** -> **
    stdlib.show :: showI * -> * -> string
    stdlib.showCharUnquoted :: char -> string
    stdlib.showFn :: showI (* -> **)
    stdlib.showbin :: int -> string
    stdlib.showchar :: showI char
    stdlib.showhex :: int -> string
    stdlib.showint :: showI int
    stdlib.showintBase :: int -> string -> int -> string
    stdlib.showlist :: showI * -> showI ([*])
    stdlib.showoct :: int -> string
    stdlib.showstring :: showI string
    stdlib.showtuple2 :: showI * -> showI ** -> showI ((*, **))
    stdlib.showunit :: showI builtin.unit
    stdlib.showword# :: showI word#
    stdlib.snd :: (*, **) -> **
    stdlib.subtract :: int -> int -> int
    stdlib.sum :: [int] -> int
    stdlib.take :: int -> [*] -> [*]
    stdlib.thenCmp :: ordering -> ordering -> ordering
    stdlib.tl :: [*] -> [*]
    stdlib.trace :: string -> * -> *
    stdlib.undef :: *
    stdlib.unlines :: [string] -> string
    stdlib.unreachable :: *
    stdlib.unsafeWriteStdErr :: string -> builtin.unit
    stdlib.writeByteStream :: word# -> string -> string
    stdlib.zip2 :: [*] -> [**] -> [(*, **)]
    stdlib.zip3 :: [*] -> [**] -> [***] -> [(*, **, ***)]

### stream
 stream.am -- implementation of streams from the paper:

    stream.seither * ** ::= stream.Sleft * | stream.Sright **
    stream.smaybe * ::= stream.Snothing | stream.Sjust *
    stream.step * ** ::= stream.Done | stream.Skip ** | stream.Yield * **
    stream.stream * ** ::= stream.Stream (** -> step * **) **
    (stream.#@) :: [*] -> int
    (stream.++@) :: [*] -> [*] -> [*]
    (stream..@) :: (** -> ***) -> (* -> **) -> * -> ***
    stream.Done :: step * **
    stream.Sjust :: * -> smaybe *
    stream.Skip :: ** -> step * **
    stream.Sleft :: * -> seither * **
    stream.Snothing :: smaybe *
    stream.Sright :: ** -> seither * **
    stream.Stream :: (** -> step * **) -> ** -> stream * **
    stream.Yield :: * -> ** -> step * **
    stream.allS :: (* -> bool) -> stream * ** -> bool
    stream.all_ :: (* -> bool) -> [*] -> bool
    stream.anyS :: (* -> bool) -> stream * ** -> bool
    stream.any_ :: (* -> bool) -> [*] -> bool
    stream.appendS :: stream * ** -> stream * *** -> stream * (seither ** ***)
    stream.cmpstream :: ordI * -> ordI (stream * **)
    stream.concatMapS :: (* -> stream ** ***) -> stream * **** -> stream ** ((****, smaybe (stream ** ***)))
    stream.concatMap_ :: (* -> [**]) -> [*] -> [**]
    stream.cycleS :: stream * ** -> stream * **
    stream.dropS :: int -> stream * ** -> stream * ((int, **))
    stream.dropWhileS :: (* -> bool) -> stream * ** -> stream * ((bool, **))
    stream.dropWhile_ :: (* -> bool) -> [*] -> [*]
    stream.drop_ :: int -> [*] -> [*]
    stream.enumerateS :: stream * ** -> stream ((int, *)) ((int, **))
    stream.filterS :: (* -> bool) -> stream * ** -> stream * **
    stream.filter_ :: (* -> bool) -> [*] -> [*]
    stream.foldl1S :: (* -> * -> *) -> stream * ** -> *
    stream.foldlS :: (* -> *** -> *) -> * -> stream *** ** -> *
    stream.foldl_ :: (* -> ** -> *) -> * -> [**] -> *
    stream.foldr1S :: (* -> * -> *) -> stream * ** -> *
    stream.foldrS :: (* -> *** -> ***) -> *** -> stream * ** -> ***
    stream.foldr_ :: (* -> ** -> **) -> ** -> [*] -> **
    stream.fromStream :: stream * ** -> [*]
    stream.interleaveS :: stream * ** -> stream * *** -> stream * ((bool, **, ***))
    stream.interleave_ :: [*] -> [*] -> [*]
    stream.iterateS :: (* -> *) -> * -> stream * *
    stream.iterate_ :: (* -> *) -> * -> [*]
    stream.lastS :: stream * ** -> *
    stream.last_ :: [*] -> *
    stream.lengthS :: stream * ** -> int
    stream.length_ :: [*] -> int
    stream.linesS :: stream char * -> stream ([char]) ((smaybe ([char]), *))
    stream.lines_ :: [char] -> [[char]]
    stream.mapS :: (* -> ***) -> stream * ** -> stream *** **
    stream.map_ :: (* -> **) -> [*] -> [**]
    stream.maxS :: ordI * -> stream * ** -> *
    stream.minS :: ordI * -> stream * ** -> *
    stream.productS :: stream int * -> int
    stream.rangeByFromS :: int -> int -> stream int word#
    stream.rangeByFrom_ :: int -> int -> [int]
    stream.rangeByS :: int -> int -> int -> stream int word#
    stream.rangeBy_ :: int -> int -> int -> [int]
    stream.rangeFromS :: int -> stream int word#
    stream.rangeFrom_ :: int -> [int]
    stream.rangeS :: int -> int -> stream int word#
    stream.range_ :: int -> int -> [int]
    stream.readByteStreamS :: word# -> stream char word#
    stream.readByteStream_ :: word# -> [char]
    stream.repS :: int -> * -> stream * word#
    stream.repeatS :: * -> stream * builtin.unit
    stream.sumS :: stream int * -> int
    stream.suncurry :: (* -> ** -> ***) -> (*, **) -> ***
    stream.takeS :: int -> stream * ** -> stream * ((int, **))
    stream.takeWhileS :: (* -> bool) -> stream * ** -> stream * **
    stream.takeWhile_ :: (* -> bool) -> [*] -> [*]
    stream.take_ :: int -> [*] -> [*]
    stream.tlS :: stream * ** -> stream * ((bool, **))
    stream.tl_ :: [*] -> [*]
    stream.toStream :: [*] -> stream * ([*])
    stream.wordsS :: stream char * -> stream ([char]) ((smaybe ([char]), *))
    stream.words_ :: [char] -> [[char]]
    stream.zip2S :: stream * ** -> stream *** **** -> stream ((*, ***)) ((**, ****, smaybe *))
    stream.zip2_ :: [*] -> [**] -> [(*, **)]
    stream.zipWithS :: (* -> ** -> ***) -> stream * **** -> stream ** ***** -> stream *** ((****, *****, smaybe *))
    stream.zipWith_ :: (* -> ** -> ***) -> [*] -> [**] -> [***]

### tardis
tardis.am -- lazy bidirectional state monad

    tardis.tardis * ** *** == (*, **) -> (***, (*, **))
    (tardis.<$>) :: (*** -> ****) -> tardis * ** *** -> tardis * ** ****
    (tardis.<&>) :: tardis * ** *** -> (*** -> ****) -> tardis * ** ****
    (tardis.<*>) :: tardis * ** (*** -> ****) -> tardis * ** *** -> tardis * ** ****
    (tardis.<<) :: tardis * ** *** -> tardis * ** **** -> tardis * ** ***
    (tardis.>=>) :: (*** -> tardis * ** ****) -> (**** -> tardis * ** *****) -> *** -> tardis * ** *****
    (tardis.>>) :: tardis * ** *** -> tardis * ** **** -> tardis * ** ****
    (tardis.>>=) :: tardis * ** *** -> (*** -> tardis * ** ****) -> tardis * ** ****
    tardis.bind2 :: tardis * ** *** -> tardis * ** **** -> (*** -> **** -> tardis * ** *****) -> tardis * ** *****
    tardis.bind3 :: tardis * ** *** -> tardis * ** **** -> tardis * ** ***** -> (*** -> **** -> ***** -> tardis * ** *7) -> tardis * ** *7
    tardis.evalState :: tardis * ** *** -> (*, **) -> ***
    tardis.execState :: tardis * ** *** -> (*, **) -> (*, **)
    tardis.filterM :: (*** -> tardis * ** bool) -> [***] -> tardis * ** ([***])
    tardis.foldM :: (**** -> *** -> tardis * ** ****) -> **** -> [***] -> tardis * ** ****
    tardis.forM :: [***] -> (*** -> tardis * ** ****) -> tardis * ** builtin.unit
    tardis.getFuture :: tardis * ** *
    tardis.getPast :: tardis * ** **
    tardis.join :: tardis * ** (tardis * ** ***) -> tardis * ** ***
    tardis.liftA2 :: (*** -> **** -> *****) -> tardis * ** *** -> tardis * ** **** -> tardis * ** *****
    tardis.liftA3 :: (*** -> **** -> ***** -> *6) -> tardis * ** *** -> tardis * ** **** -> tardis * ** ***** -> tardis * ** *6
    tardis.mapM :: (*** -> tardis * ** ****) -> [***] -> tardis * ** ([****])
    tardis.mapM_ :: (*** -> tardis * ** ****) -> [***] -> tardis * ** builtin.unit
    tardis.modifyBackwards :: (* -> *) -> tardis * ** builtin.unit
    tardis.modifyForwards :: (** -> **) -> tardis * ** builtin.unit
    tardis.pure :: *** -> tardis * ** ***
    tardis.runState :: tardis * ** *** -> (*, **) -> (***, (*, **))
    tardis.sendFuture :: ** -> tardis * ** builtin.unit
    tardis.sendPast :: * -> tardis * ** builtin.unit
    tardis.sequence_ :: [tardis * ** ***] -> tardis * ** builtin.unit

### trieMap
trieMap.am -- strict map from a key to a value, where the key is a list of elements

    trieMap.matchResult * ** ::= trieMap.Mfail | trieMap.Mkey (trie * **) ([*]) ([*]) | trieMap.Mpre (trie * **) ([*]) ([*]) | trieMap.Mpart (trie * **) ([*]) ([*]) ([*])
    trieMap.trie * ** ::= trieMap.Trie ([trieBranch * **]) (maybe **)
    trieMap.trieBranch * ** == ([*], trie * **)
    (trieMap.<$>) :: (** -> ***) -> trie * ** -> trie * ***
    (trieMap.<&>) :: trie * ** -> (** -> ***) -> trie * ***
    trieMap.Mfail :: matchResult * **
    trieMap.Mkey :: trie * ** -> [*] -> [*] -> matchResult * **
    trieMap.Mpart :: trie * ** -> [*] -> [*] -> [*] -> matchResult * **
    trieMap.Mpre :: trie * ** -> [*] -> [*] -> matchResult * **
    trieMap.Trie :: [trieBranch * **] -> maybe ** -> trie * **
    trieMap.adjust :: ordI * -> (** -> **) -> [*] -> trie * ** -> trie * **
    trieMap.delete :: ordI * -> [*] -> trie * ** -> trie * **
    trieMap.elems :: trie * ** -> [**]
    trieMap.empty :: trie * **
    trieMap.filter :: (** -> bool) -> trie * ** -> trie * **
    trieMap.findWithDefault :: ordI * -> ** -> [*] -> trie * ** -> **
    trieMap.foldl :: (*** -> ** -> ***) -> *** -> trie * ** -> ***
    trieMap.foldr :: (** -> *** -> ***) -> *** -> trie * ** -> ***
    trieMap.fromList :: ordI * -> [([*], **)] -> trie * **
    trieMap.insert :: ordI * -> [*] -> ** -> trie * ** -> trie * **
    trieMap.insertWith :: ordI * -> (** -> ** -> **) -> [*] -> ** -> trie * ** -> trie * **
    trieMap.keys :: trie * ** -> [[*]]
    trieMap.lookup :: ordI * -> [*] -> trie * ** -> maybe **
    trieMap.null :: trie * ** -> bool
    trieMap.prefix :: ordI * -> [*] -> trie * ** -> maybe **
    trieMap.tb_find :: ordI * -> [*] -> [trieBranch * **] -> matchResult * **
    trieMap.tb_match :: ordI * -> [*] -> trieBranch * ** -> matchResult * **
    trieMap.tb_modify :: ordI * -> (matchResult * ** -> maybe (trieBranch * **)) -> [*] -> [trieBranch * **] -> maybe ([trieBranch * **])
    trieMap.toList :: trie * ** -> [([*], **)]
    trieMap.union :: ordI * -> trie * ** -> trie * ** -> trie * **

### v2
 v2.am -- 2D vectors and associated operations

    v2.v2 * ::= v2.V2 * *
    (v2.<$>) :: (* -> **) -> v2 * -> v2 **
    (v2.<&>) :: v2 * -> (* -> **) -> v2 **
    (v2.<*>) :: v2 (* -> **) -> v2 * -> v2 **
    (v2.>>=) :: v2 * -> (* -> v2 **) -> v2 **
    v2.V2 :: * -> * -> v2 *
    v2.abs :: v2 int -> v2 int
    v2.add :: v2 int -> v2 int -> v2 int
    v2.cmul :: v2 int -> v2 int -> v2 int
    v2.dist :: v2 int -> v2 int -> int
    v2.div :: v2 int -> v2 int -> v2 int
    v2.foldl :: (** -> * -> **) -> ** -> v2 * -> **
    v2.foldr :: (* -> ** -> **) -> ** -> v2 * -> **
    v2.liftA2 :: (* -> ** -> ***) -> v2 * -> v2 ** -> v2 ***
    v2.max :: ordI * -> v2 * -> v2 * -> v2 *
    v2.min :: ordI * -> v2 * -> v2 * -> v2 *
    v2.mod :: v2 int -> v2 int -> v2 int
    v2.mul :: v2 int -> v2 int -> v2 int
    v2.neg :: v2 int -> v2 int
    v2.product :: v2 int -> int
    v2.pure :: * -> v2 *
    v2.quot :: v2 int -> v2 int -> v2 int
    v2.rem :: v2 int -> v2 int -> v2 int
    v2.sequence :: [v2 *] -> v2 ([*])
    v2.signum :: v2 int -> v2 int
    v2.sub :: v2 int -> v2 int -> v2 int
    v2.sum :: v2 int -> int

### v3
 v3.am -- 3D vectors and associated operations

    v3.v3 * ::= v3.V3 * * *
    (v3.<$>) :: (* -> **) -> v3 * -> v3 **
    (v3.<&>) :: v3 * -> (* -> **) -> v3 **
    (v3.<*>) :: v3 (* -> **) -> v3 * -> v3 **
    (v3.>>=) :: v3 * -> (* -> v3 **) -> v3 **
    v3.V3 :: * -> * -> * -> v3 *
    v3.abs :: v3 int -> v3 int
    v3.add :: v3 int -> v3 int -> v3 int
    v3.cross :: v3 int -> v3 int -> v3 int
    v3.dist :: v3 int -> v3 int -> int
    v3.div :: v3 int -> v3 int -> v3 int
    v3.foldl :: (** -> * -> **) -> ** -> v3 * -> **
    v3.foldr :: (* -> ** -> **) -> ** -> v3 * -> **
    v3.liftA2 :: (* -> ** -> ***) -> v3 * -> v3 ** -> v3 ***
    v3.max :: ordI * -> v3 * -> v3 * -> v3 *
    v3.min :: ordI * -> v3 * -> v3 * -> v3 *
    v3.mod :: v3 int -> v3 int -> v3 int
    v3.mul :: v3 int -> v3 int -> v3 int
    v3.neg :: v3 int -> v3 int
    v3.product :: v3 int -> int
    v3.pure :: * -> v3 *
    v3.quot :: v3 int -> v3 int -> v3 int
    v3.rem :: v3 int -> v3 int -> v3 int
    v3.sequence :: [v3 *] -> v3 ([*])
    v3.signum :: v3 int -> v3 int
    v3.sub :: v3 int -> v3 int -> v3 int
    v3.sum :: v3 int -> int

### vector
vector.am -- immutable and mutable vectors, and the ST monad for sequencing in-place modification

    vector.mvector * ::= vector.MVector int word#
    vector.vector * ::= vector.Vector int word#
    vector.st * == state builtin.unit *
    vector.stRef * == mvector *
    (vector.!!) :: vector * -> int -> *
    (vector.//) :: vector * -> [(int, *)] -> vector *
    (vector.<$>) :: (* -> **) -> vector * -> vector **
    (vector.<&>) :: vector * -> (* -> **) -> vector **
    vector.MVector :: int -> word# -> mvector *
    vector.Vector :: int -> word# -> vector *
    vector.all :: (* -> bool) -> vector * -> bool
    vector.any :: (* -> bool) -> vector * -> bool
    vector.append :: vector * -> vector * -> vector *
    vector.clone :: mvector * -> state ** (mvector *)
    vector.cmpvector :: ordI * -> vector * -> vector * -> ordering
    vector.fill :: mvector * -> * -> state ** builtin.unit
    vector.filter :: (* -> bool) -> vector * -> vector *
    vector.find :: (* -> bool) -> vector * -> maybe *
    vector.first :: vector * -> *
    vector.fmapWithIndex :: (int -> * -> **) -> vector * -> vector **
    vector.foldl :: (** -> * -> **) -> ** -> vector * -> **
    vector.foldr :: (* -> ** -> **) -> ** -> vector * -> **
    vector.freeze :: mvector * -> state ** (vector *)
    vector.fromList :: [*] -> vector *
    vector.fromStream :: int -> stream * ** -> vector *
    vector.generate :: int -> (int -> *) -> vector *
    vector.index :: vector * -> int -> *
    vector.iterateN :: int -> (* -> *) -> * -> vector *
    vector.last :: vector * -> *
    vector.length :: vector * -> int
    vector.max :: ordI * -> vector * -> *
    vector.min :: ordI * -> vector * -> *
    vector.mlength :: mvector * -> int
    vector.modify :: mvector * -> (* -> *) -> int -> state ** builtin.unit
    vector.modifySTRef :: stRef * -> (* -> *) -> st builtin.unit
    vector.newSTRef :: * -> st (stRef *)
    vector.product :: vector int -> int
    vector.read :: mvector * -> int -> state ** *
    vector.readSTRef :: stRef * -> st *
    vector.rep :: int -> * -> vector *
    vector.replace :: vector * -> [(int, *)] -> vector *
    vector.runST :: st * -> *
    vector.runSTVector :: (mvector * -> st **) -> vector * -> vector *
    vector.safeIndex :: int -> int -> int
    vector.search :: (* -> * -> ordering) -> vector * -> * -> maybe ((int, *))
    vector.showvector :: showI * -> vector * -> string
    vector.singleton :: * -> vector *
    vector.sortBy :: ordI * -> vector * -> vector *
    vector.sum :: vector int -> int
    vector.thaw :: vector * -> mvector *
    vector.toList :: vector * -> [*]
    vector.toStream :: vector * -> stream * int
    vector.unsafeFreeze :: mvector * -> state ** (vector *)
    vector.unsafeIndex :: vector * -> int -> *
    vector.unsafeModify :: mvector * -> (* -> *) -> int -> state ** builtin.unit
    vector.unsafeRead :: mvector * -> int -> state ** *
    vector.unsafeReplace :: mvector * -> [(int, *)] -> state ** builtin.unit
    vector.unsafeThaw :: vector * -> mvector *
    vector.unsafeWrite :: mvector * -> int -> * -> state ** builtin.unit
    vector.write :: mvector * -> int -> * -> state ** builtin.unit
    vector.writeSTRef :: stRef * -> * -> st builtin.unit
    vector.zipWith :: (* -> ** -> ***) -> vector * -> vector ** -> vector ***

### zipper
zipper.am -- implementation of a list with a cursor

    zipper.zipper * ::= zipper.Zipper ([*]) ! ([*]) !
    (zipper.<$>) :: (* -> **) -> zipper * -> zipper **
    (zipper.<&>) :: zipper * -> (* -> **) -> zipper **
    zipper.Zipper :: [*] -> [*] -> zipper *
    zipper.begin :: zipper * -> zipper *
    zipper.beginp :: zipper * -> bool
    zipper.cursor :: zipper * -> *
    zipper.delete :: zipper * -> zipper *
    zipper.empty :: zipper *
    zipper.end :: zipper * -> zipper *
    zipper.endp :: zipper * -> bool
    zipper.fold :: (** -> * -> **) -> ** -> zipper * -> **
    zipper.fromList :: [*] -> zipper *
    zipper.insert :: * -> zipper * -> zipper *
    zipper.left :: zipper * -> zipper *
    zipper.modify :: (* -> *) -> zipper * -> zipper *
    zipper.null :: zipper * -> bool
    zipper.pop :: zipper * -> zipper *
    zipper.push :: * -> zipper * -> zipper *
    zipper.right :: zipper * -> zipper *
    zipper.singleton :: * -> zipper *
    zipper.toList :: zipper * -> [*]
