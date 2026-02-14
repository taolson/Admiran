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

    astar.mstate * ** == (**, s_set ((int, *)), m_map * int, m_map * int, m_map * *)
    astar.rstate * ** == (ordI *, ordI ((int, *)), * -> bool, (*, **) -> ([*], **), * -> * -> int, * -> int)
    astar.aStarReachable :: ordI * -> * -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> m_map * *
    astar.aStarSolve :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> ([*], **)
    astar.addMoves :: rstate * ** -> mstate * ** -> * -> [*] -> mstate * **
    astar.backtrace :: rstate * ** -> mstate * ** -> * -> ([*], mstate * **)
    astar.getCost :: ordI * -> * -> m_map * int -> int
    astar.maxCost :: int
    astar.solve :: rstate * ** -> mstate * ** -> ([*], mstate * **)

### avl
avl.am -- AVL tree

    avl.avlTree * ::= avl.AVLLeaf | avl.AVLNode * (avlTree *) (avlTree *) int
    avl.AVLLeaf :: avlTree *
    avl.AVLNode :: * -> avlTree * -> avlTree * -> int -> avlTree *
    avl.a_balance :: avlTree * -> avlTree *
    avl.a_delete :: ordI * -> * -> avlTree * -> avlTree *
    avl.a_empty :: avlTree *
    avl.a_first :: avlTree * -> *
    avl.a_fmap :: ordI ** -> (* -> **) -> avlTree * -> avlTree **
    avl.a_foldl :: (** -> * -> **) -> ** -> avlTree * -> **
    avl.a_foldr :: (* -> ** -> **) -> ** -> avlTree * -> **
    avl.a_fromList :: ordI * -> [*] -> avlTree *
    avl.a_insert :: ordI * -> * -> avlTree * -> avlTree *
    avl.a_last :: avlTree * -> *
    avl.a_member :: ordI * -> * -> avlTree * -> bool
    avl.a_moveR :: avlTree * -> avlTree * -> avlTree *
    avl.a_null :: avlTree * -> bool
    avl.a_singleton :: * -> avlTree *
    avl.a_size :: avlTree * -> int
    avl.a_toList :: avlTree * -> [*]
    avl.a_union :: ordI * -> avlTree * -> avlTree * -> avlTree *
    avl.balanceL :: avlTree * -> avlTree *
    avl.balanceR :: avlTree * -> avlTree *
    avl.cmpavlTree :: ordI * -> ordI (avlTree *)
    avl.computeHeight :: avlTree * -> avlTree *
    avl.delta :: avlTree * -> int
    avl.height :: avlTree * -> int
    avl.rotL :: avlTree * -> avlTree *
    avl.rotR :: avlTree * -> avlTree *

### bag
bag.am -- implementation of a strict multiset using AVL trees

    bag.b_bag * == m_map * int
    bag.b_delete :: ordI * -> * -> b_bag * -> b_bag *
    bag.b_deleteTimes :: ordI * -> * -> int -> b_bag * -> b_bag *
    bag.b_fromCountList :: ordI * -> [(*, int)] -> b_bag *
    bag.b_fromList :: ordI * -> [*] -> b_bag *
    bag.b_insert :: ordI * -> * -> b_bag * -> b_bag *
    bag.b_insertTimes :: ordI * -> * -> int -> b_bag * -> b_bag *
    bag.b_singleton :: * -> b_bag *
    bag.b_union :: ordI * -> b_bag * -> b_bag * -> b_bag *
    bag.b_withKeys :: ordI * -> [*] -> b_bag *

### base
base.am -- common extensions to Admiran's standard library

    (base.!>) :: * -> (* -> **) -> **
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
    base.padl :: int -> string -> string
    base.padr :: int -> string -> string
    base.pair :: * -> ** -> (*, **)
    base.partition :: (* -> bool) -> [*] -> ([*], [*])
    base.permutations :: [*] -> [[*]]
    base.permutationsWithRep :: [*] -> [[*]]
    base.replicate :: int -> * -> [*]
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

    bfs.backtrace :: ordI * -> * -> ** -> m_map * * -> ([*], **)
    bfs.bfsSolve :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> ([*], **)

### bitSet
bitSet.am -- representation of a set of small (< 64) natural numbers using an int, as an abstract type

    bitSet.bs_all :: int -> bitSet
    bitSet.bs_delete :: int -> bitSet -> bitSet
    bitSet.bs_difference :: bitSet -> bitSet -> bitSet
    bitSet.bs_empty :: bitSet
    bitSet.bs_first :: bitSet -> int
    bitSet.bs_fromInt :: int -> bitSet
    bitSet.bs_fromList :: [int] -> bitSet
    bitSet.bs_insert :: int -> bitSet -> bitSet
    bitSet.bs_intersect :: bitSet -> bitSet -> bitSet
    bitSet.bs_last :: bitSet -> int
    bitSet.bs_member :: int -> bitSet -> bool
    bitSet.bs_null :: bitSet -> bool
    bitSet.bs_singleton :: int -> bitSet
    bitSet.bs_size :: bitSet -> int
    bitSet.bs_toInt :: bitSet -> int
    bitSet.bs_toList :: bitSet -> [int]
    bitSet.bs_union :: bitSet -> bitSet -> bitSet
    bitSet.cmpbitSet :: bitSet -> bitSet -> ordering
    bitSet.showbitSet :: bitSet -> string

### dequeue
dequeue.am -- a double-ended queue of elements that allows quick insertion / deletion at both ends

    dequeue.dequeue * ::= dequeue.FT0 | dequeue.FT1 * | dequeue.FT2 * * | dequeue.FT3 * * * | dequeue.FTN (dequeue *) (dequeue (dequeue *)) (dequeue *)
    dequeue.FT0 :: dequeue *
    dequeue.FT1 :: * -> dequeue *
    dequeue.FT2 :: * -> * -> dequeue *
    dequeue.FT3 :: * -> * -> * -> dequeue *
    dequeue.FTN :: dequeue * -> dequeue (dequeue *) -> dequeue * -> dequeue *
    dequeue.dq_addL :: * -> dequeue * -> dequeue *
    dequeue.dq_addR :: * -> dequeue * -> dequeue *
    dequeue.dq_empty :: dequeue *
    dequeue.dq_fromList :: [*] -> dequeue *
    dequeue.dq_null :: dequeue * -> bool
    dequeue.dq_singleton :: * -> dequeue *
    dequeue.dq_size :: dequeue * -> int
    dequeue.dq_toList :: dequeue * -> [*]
    dequeue.dq_viewL :: dequeue * -> maybe ((*, dequeue *))
    dequeue.dq_viewR :: dequeue * -> maybe ((*, dequeue *))
    dequeue.isSat :: dequeue * -> bool

### either
either.am -- sum type of two distinct types

    either.either * ** ::= either.Left * | either.Right **
    either.Left :: * -> either * **
    either.Right :: ** -> either * **
    either.e_apply :: either * (** -> ***) -> either * ** -> either * ***
    either.e_bind :: either * ** -> (** -> either * ***) -> either * ***
    either.e_fmap :: (** -> ***) -> either * ** -> either * ***
    either.e_foldM :: (** -> * -> either *** **) -> ** -> [*] -> either *** **
    either.e_kbind :: (* -> either ** ***) -> (*** -> either ** ****) -> * -> either ** ****
    either.e_liftA2 :: (** -> *** -> ****) -> either * ** -> either * *** -> either * ****
    either.e_mapM :: (* -> either ** ***) -> [*] -> either ** ([***])
    either.e_pure :: ** -> either * **
    either.e_sequence :: [either * **] -> either * ([**])
    either.eitherf :: (* -> ***) -> (** -> ***) -> either * ** -> ***
    either.fromEither :: ** -> either * ** -> **
    either.isLeft :: either * ** -> bool
    either.isRight :: either * ** -> bool
    either.partitionEithers :: [either * **] -> ([*], [**])

### fix16
fixed-point representation with 16-bit fractional part, as a substitute for floats

    (fix16.*.) :: fix16 -> fix16 -> fix16
    (fix16.+.) :: fix16 -> fix16 -> fix16
    (fix16.-.) :: fix16 -> fix16 -> fix16
    (fix16./.) :: fix16 -> fix16 -> fix16
    fix16.cmpfix16 :: fix16 -> fix16 -> ordering
    fix16.fix16Frac :: fix16 -> int
    fix16.fix16Int :: fix16 -> int
    fix16.fix16val :: string -> fix16
    fix16.showfix16 :: fix16 -> string
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
    heap.h_empty :: heap *
    heap.h_fromList :: ordI * -> [*] -> heap *
    heap.h_insert :: ordI * -> * -> heap * -> heap *
    heap.h_null :: heap * -> bool
    heap.h_singleton :: * -> heap *
    heap.h_size :: heap * -> int
    heap.h_toList :: heap * -> [*]
    heap.h_union :: ordI * -> heap * -> heap * -> heap *
    heap.h_viewMin :: ordI * -> heap * -> maybe ((*, heap *))
    heap.ins :: ordI * -> htree * -> hforest * -> hforest *
    heap.link :: ordI * -> htree * -> htree * -> htree *
    heap.rank :: htree * -> int
    heap.root :: htree * -> *
    heap.skewInsert :: ordI * -> htree * -> hforest * -> hforest *
    heap.skewLink :: ordI * -> htree * -> htree * -> htree * -> htree *
    heap.skewMeld :: ordI * -> hforest * -> hforest * -> hforest *
    heap.splitForest :: int -> hforest * -> hforest * -> hforest * -> (hforest *, hforest *, hforest *)
    heap.t_toList :: htree * -> [*]
    heap.unionUniq :: ordI * -> hforest * -> hforest * -> hforest *
    heap.uniqify :: ordI * -> hforest * -> hforest *
    heap.viewMin :: ordI * -> hforest * -> maybe ((htree *, hforest *))

### io
io.am -- the IO monad for sequencing access to the outside world

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
    io.readFileStream :: handle -> io string
    io.stderr :: handle
    io.stdin :: handle
    io.stdout :: handle
    io.systemCmd :: string -> io int
    io.time :: (* -> **) -> * -> io ((**, int))
    io.unsafePerformIO :: io * -> *
    io.writeFile :: string -> string -> io builtin.unit
    io.writeFileStream :: handle -> string -> io builtin.unit

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

    map.m_map * ** == avlTree ((*, **))
    map.m_adjust :: ordI * -> (** -> **) -> * -> m_map * ** -> m_map * **
    map.m_alter :: ordI * -> (maybe ** -> maybe **) -> * -> m_map * ** -> m_map * **
    map.m_delete :: ordI * -> * -> m_map * ** -> m_map * **
    map.m_elems :: m_map * ** -> [**]
    map.m_filter :: ordI * -> (** -> bool) -> m_map * ** -> m_map * **
    map.m_filterWithKey :: ordI * -> ((*, **) -> bool) -> m_map * ** -> m_map * **
    map.m_findWithDefault :: ordI * -> ** -> * -> m_map * ** -> **
    map.m_fmap :: (** -> ***) -> m_map * ** -> m_map * ***
    map.m_fmapWithKey :: (* -> ** -> ***) -> m_map * ** -> m_map * ***
    map.m_foldl :: (*** -> ** -> ***) -> *** -> m_map * ** -> ***
    map.m_foldr :: (** -> *** -> ***) -> *** -> m_map * ** -> ***
    map.m_fromList :: ordI * -> [(*, **)] -> m_map * **
    map.m_insert :: ordI * -> * -> ** -> m_map * ** -> m_map * **
    map.m_insertWith :: ordI * -> (** -> ** -> **) -> * -> ** -> m_map * ** -> m_map * **
    map.m_keys :: m_map * ** -> [*]
    map.m_keysSet :: m_map * ** -> avlTree *
    map.m_lookup :: ordI * -> * -> m_map * ** -> maybe **
    map.m_mapAccumL :: (**** -> ** -> (****, ***)) -> **** -> m_map * ** -> (****, m_map * ***)
    map.m_member :: ordI * -> * -> m_map * ** -> bool
    map.m_singleton :: * -> ** -> m_map * **
    map.m_union :: ordI * -> m_map * ** -> m_map * ** -> m_map * **

### maybe
maybe.am -- sum type of "Nothing" and another type

    maybe.maybe * ::= maybe.Nothing | maybe.Just *
    maybe.Just :: * -> maybe *
    maybe.Nothing :: maybe *
    maybe.catMaybes :: [maybe *] -> [*]
    maybe.fromJust :: maybe * -> *
    maybe.fromMaybe :: * -> maybe * -> *
    maybe.fromMaybef :: ** -> (* -> **) -> maybe * -> **
    maybe.isJust :: maybe * -> bool
    maybe.isNothing :: maybe * -> bool
    maybe.mapMaybe :: (* -> maybe **) -> [*] -> [**]
    maybe.mb_alt :: maybe * -> maybe * -> maybe *
    maybe.mb_apply :: maybe (* -> **) -> maybe * -> maybe **
    maybe.mb_bind :: maybe * -> (* -> maybe **) -> maybe **
    maybe.mb_filterM :: (* -> maybe bool) -> [*] -> maybe ([*])
    maybe.mb_fmap :: (* -> **) -> maybe * -> maybe **
    maybe.mb_foldM :: (** -> * -> maybe **) -> ** -> [*] -> maybe **
    maybe.mb_kbind :: (* -> maybe **) -> (** -> maybe ***) -> * -> maybe ***
    maybe.mb_left :: maybe * -> maybe ** -> maybe *
    maybe.mb_liftA2 :: (* -> ** -> ***) -> maybe * -> maybe ** -> maybe ***
    maybe.mb_mapM :: (* -> maybe **) -> [*] -> maybe ([**])
    maybe.mb_pure :: * -> maybe *
    maybe.mb_right :: maybe * -> maybe ** -> maybe **
    maybe.mb_sequence :: [maybe *] -> maybe ([*])

### maybeState
maybeState.am -- functor / applicative / monad / alternative for a state monad augmented with maybe

    maybeState.maybeState * ** == state * (maybe **)
    maybeState.mst_alt :: maybeState * ** -> maybeState * ** -> maybeState * **
    maybeState.mst_apply :: maybeState * (** -> ***) -> maybeState * ** -> maybeState * ***
    maybeState.mst_bind :: maybeState * ** -> (** -> maybeState * ***) -> maybeState * ***
    maybeState.mst_bind2 :: maybeState * ** -> maybeState * *** -> (** -> *** -> maybeState * ****) -> maybeState * ****
    maybeState.mst_bind3 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> (** -> *** -> **** -> maybeState * *****) -> maybeState * *****
    maybeState.mst_bind4 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> (** -> *** -> **** -> ***** -> maybeState * *6) -> maybeState * *6
    maybeState.mst_fail :: maybeState * **
    maybeState.mst_filterM :: (** -> maybeState * bool) -> [**] -> maybeState * ([**])
    maybeState.mst_fmap :: (** -> ***) -> maybeState * ** -> maybeState * ***
    maybeState.mst_foldM :: (*** -> ** -> maybeState * ***) -> *** -> [**] -> maybeState * ***
    maybeState.mst_forM :: [**] -> (** -> maybeState * ***) -> maybeState * builtin.unit
    maybeState.mst_get :: maybeState * *
    maybeState.mst_join :: maybeState * (maybeState * **) -> maybeState * **
    maybeState.mst_kbind :: (** -> maybeState * ***) -> (*** -> maybeState * ****) -> ** -> maybeState * ****
    maybeState.mst_left :: maybeState * ** -> maybeState * *** -> maybeState * **
    maybeState.mst_lift :: state * ** -> maybeState * **
    maybeState.mst_liftA2 :: (** -> *** -> ****) -> maybeState * ** -> maybeState * *** -> maybeState * ****
    maybeState.mst_liftA3 :: (** -> *** -> **** -> *****) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * *****
    maybeState.mst_liftA4 :: (** -> *** -> **** -> ***** -> *6) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> maybeState * *6
    maybeState.mst_many :: maybeState * ** -> maybeState * ([**])
    maybeState.mst_mapM :: (** -> maybeState * ***) -> [**] -> maybeState * ([***])
    maybeState.mst_mapM_ :: (** -> maybeState * ***) -> [**] -> maybeState * builtin.unit
    maybeState.mst_maybe :: maybe ** -> maybeState * **
    maybeState.mst_modify :: (* -> *) -> maybeState * builtin.unit
    maybeState.mst_pure :: ** -> maybeState * **
    maybeState.mst_put :: * -> maybeState * builtin.unit
    maybeState.mst_right :: maybeState * ** -> maybeState * *** -> maybeState * ***
    maybeState.mst_sequence :: [maybeState * **] -> maybeState * ([**])
    maybeState.mst_sequence_ :: [maybeState * **] -> maybeState * builtin.unit
    maybeState.mst_some :: maybeState * ** -> maybeState * ([**])

### memo
memo.am -- memoization of a single-argument function with a state map

    memo.memoSt * ** == state ((ordI *, m_map * **)) **
    memo.memo :: (* -> memoSt * **) -> * -> memoSt * **

### memoTrie
memoTrie.am -- memoization of functions using a lazy mapping of indices in a trie

    memoTrie.tree * ::= memoTrie.Tree (tree *) * (tree *)
    memoTrie.Tree :: tree * -> * -> tree * -> tree *
    memoTrie.memo :: (* -> int) -> (int -> *) -> (* -> **) -> * -> **
    memoTrie.memochar :: (char -> *) -> char -> *
    memoTrie.memofix :: ((* -> **) -> * -> **) -> ((* -> **) -> * -> **) -> * -> **
    memoTrie.memoint :: (int -> *) -> int -> *
    memoTrie.memolist :: ((* -> [*] -> **) -> * -> [*] -> **) -> ([*] -> **) -> [*] -> **
    memoTrie.memopair :: ((* -> ** -> ***) -> * -> ** -> ***) -> ((** -> ***) -> ** -> ***) -> ((*, **) -> ***) -> (*, **) -> ***
    memoTrie.memostring :: (string -> *) -> string -> *
    memoTrie.t_fmap :: (* -> **) -> tree * -> tree **

### parser
parser.am -- a parser combinator library for strings, based upon the maybeState monad, which tracks line and column position for error reporting

    parser.parser * == maybeState psSt *
    parser.psSt == (word#, word#, [char])
    parser.p_alt :: parser * -> parser * -> parser *
    parser.p_any :: parser char
    parser.p_anyOf :: [char] -> parser char
    parser.p_char :: char -> parser char
    parser.p_comma :: parser char
    parser.p_cons :: parser * -> parser ([*]) -> parser ([*])
    parser.p_count :: int -> parser * -> parser ([*])
    parser.p_digit :: parser char
    parser.p_end :: parser builtin.unit
    parser.p_error :: psSt -> [char]
    parser.p_inAngles :: parser * -> parser *
    parser.p_inBraces :: parser * -> parser *
    parser.p_inBrackets :: parser * -> parser *
    parser.p_inParens :: parser * -> parser *
    parser.p_int :: parser int
    parser.p_intlist :: parser ([int])
    parser.p_letter :: parser char
    parser.p_many :: parser * -> parser ([*])
    parser.p_manySepBy :: parser * -> parser ** -> parser ([**])
    parser.p_manyUntil :: parser * -> parser ** -> parser ([*])
    parser.p_noneOf :: [char] -> parser char
    parser.p_not :: parser * -> parser builtin.unit
    parser.p_notChar :: char -> parser char
    parser.p_optional :: parser * -> parser (maybe *)
    parser.p_peek :: parser * -> parser builtin.unit
    parser.p_posint :: parser int
    parser.p_satisfy :: (char -> bool) -> parser char
    parser.p_skipUntil :: parser * -> parser ** -> parser **
    parser.p_some :: parser * -> parser ([*])
    parser.p_someSepBy :: parser * -> parser ** -> parser ([**])
    parser.p_space :: parser char
    parser.p_spaces :: parser ([char])
    parser.p_string :: [char] -> parser ([char])
    parser.p_word :: parser ([char])
    parser.parse :: parser * -> [char] -> (maybe *, psSt)
    parser.readIntlist :: string -> [int]

### rws
rws.am -- reader+writer+state functor/applicative/monad with strict writer and state

    rws.rws * ** *** **** == * -> *** -> [**] -> (****, ***, [**])
    rws.rws_apply :: rws * ** *** (**** -> *****) -> rws * ** *** **** -> rws * ** *** *****
    rws.rws_ask :: rws * ** *** *
    rws.rws_asks :: (* -> ****) -> rws * ** *** ****
    rws.rws_bind :: rws * ** *** **** -> (**** -> rws * ** *** *****) -> rws * ** *** *****
    rws.rws_bind2 :: rws * ** *** **** -> rws * ** *** ***** -> (**** -> ***** -> rws * ** *** *6) -> rws * ** *** *6
    rws.rws_bind3 :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> (**** -> ***** -> *6 -> rws * ** *** *7) -> rws * ** *** *7
    rws.rws_evalRWS :: rws * ** *** **** -> * -> *** -> [**] -> (****, [**])
    rws.rws_execRWS :: rws * ** *** **** -> * -> *** -> [**] -> (***, [**])
    rws.rws_filterM :: (**** -> rws * ** *** bool) -> [****] -> rws * ** *** ([****])
    rws.rws_fmap :: (**** -> *****) -> rws * ** *** **** -> rws * ** *** *****
    rws.rws_foldM :: (***** -> **** -> rws * ** *** *****) -> ***** -> [****] -> rws * ** *** *****
    rws.rws_forM :: [****] -> (**** -> rws * ** *** *****) -> rws * ** *** builtin.unit
    rws.rws_get :: rws * ** *** ***
    rws.rws_join :: rws * ** *** (rws * ** *** ****) -> rws * ** *** ****
    rws.rws_kbind :: (**** -> rws * ** *** *****) -> (***** -> rws * ** *** *6) -> **** -> rws * ** *** *6
    rws.rws_left :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** ****
    rws.rws_liftA2 :: (**** -> ***** -> *6) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6
    rws.rws_liftA3 :: (**** -> ***** -> *6 -> *7) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> rws * ** *** *7
    rws.rws_liftA4 :: (**** -> ***** -> *6 -> *7 -> *8) -> rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *6 -> rws * ** *** *7 -> rws * ** *** *8
    rws.rws_local :: (* -> *) -> rws * ** *** **** -> rws * ** *** ****
    rws.rws_mapM :: (**** -> rws * ** *** *****) -> [****] -> rws * ** *** ([*****])
    rws.rws_mapM_ :: (**** -> rws * ** *** *****) -> [****] -> rws * ** *** builtin.unit
    rws.rws_modify :: (*** -> ***) -> rws * ** *** builtin.unit
    rws.rws_pure :: **** -> rws * ** *** ****
    rws.rws_put :: *** -> rws * ** *** builtin.unit
    rws.rws_right :: rws * ** *** **** -> rws * ** *** ***** -> rws * ** *** *****
    rws.rws_runRWS :: rws * ** *** **** -> * -> *** -> [**] -> (****, ***, [**])
    rws.rws_sequence :: [rws * ** *** ****] -> rws * ** *** ([****])
    rws.rws_tell :: ** -> rws * ** *** builtin.unit
    rws.rws_tells :: ([**] -> [**]) -> rws * ** *** builtin.unit

### set
set.am -- implementation of a strict set using AVL trees

    set.s_set * == avlTree *
    set.s_difference :: ordI * -> s_set * -> s_set * -> s_set *
    set.s_filter :: ordI * -> (* -> bool) -> s_set * -> s_set *
    set.s_fmap :: ordI ** -> (* -> **) -> s_set * -> s_set **
    set.s_fromList :: ordI * -> [*] -> s_set *
    set.s_insert :: ordI * -> * -> s_set * -> s_set *
    set.s_insertIfAbsent :: ordI * -> * -> s_set * -> maybe (s_set *)
    set.s_intersect :: ordI * -> s_set * -> s_set * -> s_set *
    set.s_lookupGE :: ordI * -> * -> s_set * -> maybe *
    set.s_lookupLE :: ordI * -> * -> s_set * -> maybe *
    set.s_union :: ordI * -> s_set * -> s_set * -> s_set *
    set.s_viewMax :: s_set * -> maybe ((*, s_set *))
    set.s_viewMin :: s_set * -> maybe ((*, s_set *))

### state
state.am -- strict state functor / applicative / monad

    state.state * ** == * -> (**, *)
    state.st_apply :: state * (** -> ***) -> state * ** -> state * ***
    state.st_bind :: state * ** -> (** -> state * ***) -> state * ***
    state.st_bind2 :: state * ** -> state * *** -> (** -> *** -> state * ****) -> state * ****
    state.st_bind3 :: state * ** -> state * *** -> state * **** -> (** -> *** -> **** -> state * *****) -> state * *****
    state.st_bind4 :: state * ** -> state * *** -> state * **** -> state * ***** -> (** -> *** -> **** -> ***** -> state * *6) -> state * *6
    state.st_bind5 :: state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> (** -> *** -> **** -> ***** -> *6 -> state * *7) -> state * *7
    state.st_evalState :: state * ** -> * -> **
    state.st_execState :: state * ** -> * -> *
    state.st_filterM :: (** -> state * bool) -> [**] -> state * ([**])
    state.st_fmap :: (** -> ***) -> state * ** -> state * ***
    state.st_foldM :: (*** -> ** -> state * ***) -> *** -> [**] -> state * ***
    state.st_forM :: [**] -> (** -> state * ***) -> state * builtin.unit
    state.st_get :: state * *
    state.st_join :: state * (state * **) -> state * **
    state.st_kbind :: (** -> state * ***) -> (*** -> state * ****) -> ** -> state * ****
    state.st_left :: state * ** -> state * *** -> state * **
    state.st_liftA2 :: (** -> *** -> ****) -> state * ** -> state * *** -> state * ****
    state.st_liftA3 :: (** -> *** -> **** -> *****) -> state * ** -> state * *** -> state * **** -> state * *****
    state.st_liftA4 :: (** -> *** -> **** -> ***** -> *6) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6
    state.st_liftA5 :: (** -> *** -> **** -> ***** -> *6 -> *7) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> state * *7
    state.st_liftA6 :: (** -> *** -> **** -> ***** -> *6 -> *7 -> *8) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * *6 -> state * *7 -> state * *8
    state.st_mapM :: (** -> state * ***) -> [**] -> state * ([***])
    state.st_mapM_ :: (** -> state * ***) -> [**] -> state * builtin.unit
    state.st_modify :: (* -> *) -> state * builtin.unit
    state.st_pure :: ** -> state * **
    state.st_put :: * -> state * builtin.unit
    state.st_right :: state * ** -> state * *** -> state * ***
    state.st_runState :: state * ** -> * -> (**, *)
    state.st_sequence :: [state * **] -> state * ([**])
    state.st_sequence_ :: [state * **] -> state * builtin.unit

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
    stdlib.showCharEscaped :: char -> string
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
    stream.dropS :: int -> stream * ** -> stream * ((int, **))
    stream.dropWhileS :: (* -> bool) -> stream * ** -> stream * ((bool, **))
    stream.dropWhile_ :: (* -> bool) -> [*] -> [*]
    stream.drop_ :: int -> [*] -> [*]
    stream.enumerateS :: stream * ** -> stream ((int, *)) ((int, **))
    stream.filterS :: (* -> bool) -> stream * ** -> stream * **
    stream.filter_ :: (* -> bool) -> [*] -> [*]
    stream.foldlS :: (* -> *** -> *) -> * -> stream *** ** -> *
    stream.foldl_ :: (* -> ** -> *) -> * -> [**] -> *
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

### trieMap
trieMap.am -- strict map from a key to a value, where the key is a list of elements

    trieMap.matchResult * ** ::= trieMap.Mfail | trieMap.Mkey (trie * **) ([*]) ([*]) | trieMap.Mpre (trie * **) ([*]) ([*]) | trieMap.Mpart (trie * **) ([*]) ([*]) ([*])
    trieMap.trie * ** ::= trieMap.Trie ([trieBranch * **]) (maybe **)
    trieMap.trieBranch * ** == ([*], trie * **)
    trieMap.Mfail :: matchResult * **
    trieMap.Mkey :: trie * ** -> [*] -> [*] -> matchResult * **
    trieMap.Mpart :: trie * ** -> [*] -> [*] -> [*] -> matchResult * **
    trieMap.Mpre :: trie * ** -> [*] -> [*] -> matchResult * **
    trieMap.Trie :: [trieBranch * **] -> maybe ** -> trie * **
    trieMap.t_adjust :: ordI * -> (** -> **) -> [*] -> trie * ** -> trie * **
    trieMap.t_delete :: ordI * -> [*] -> trie * ** -> trie * **
    trieMap.t_elems :: trie * ** -> [**]
    trieMap.t_empty :: trie * **
    trieMap.t_filter :: (** -> bool) -> trie * ** -> trie * **
    trieMap.t_findWithDefault :: ordI * -> ** -> [*] -> trie * ** -> **
    trieMap.t_fmap :: (** -> ***) -> trie * ** -> trie * ***
    trieMap.t_foldl :: (*** -> ** -> ***) -> *** -> trie * ** -> ***
    trieMap.t_foldr :: (** -> *** -> ***) -> *** -> trie * ** -> ***
    trieMap.t_fromList :: ordI * -> [([*], **)] -> trie * **
    trieMap.t_insert :: ordI * -> [*] -> ** -> trie * ** -> trie * **
    trieMap.t_insertWith :: ordI * -> (** -> ** -> **) -> [*] -> ** -> trie * ** -> trie * **
    trieMap.t_keys :: trie * ** -> [[*]]
    trieMap.t_lookup :: ordI * -> [*] -> trie * ** -> maybe **
    trieMap.t_null :: trie * ** -> bool
    trieMap.t_prefix :: ordI * -> [*] -> trie * ** -> maybe **
    trieMap.t_toList :: trie * ** -> [([*], **)]
    trieMap.t_union :: ordI * -> trie * ** -> trie * ** -> trie * **
    trieMap.tb_find :: ordI * -> [*] -> [trieBranch * **] -> matchResult * **
    trieMap.tb_match :: ordI * -> [*] -> trieBranch * ** -> matchResult * **
    trieMap.tb_modify :: ordI * -> (matchResult * ** -> maybe (trieBranch * **)) -> [*] -> [trieBranch * **] -> maybe ([trieBranch * **])

### v2
 v2.am -- 2D vectors and associated operations

    v2.v2 * ::= v2.V2 * *
    v2.V2 :: * -> * -> v2 *
    v2.v2_abs :: v2 int -> v2 int
    v2.v2_add :: v2 int -> v2 int -> v2 int
    v2.v2_apply :: v2 (* -> **) -> v2 * -> v2 **
    v2.v2_bind :: v2 * -> (* -> v2 **) -> v2 **
    v2.v2_cmul :: v2 int -> v2 int -> v2 int
    v2.v2_dist :: v2 int -> v2 int -> int
    v2.v2_div :: v2 int -> v2 int -> v2 int
    v2.v2_fmap :: (* -> **) -> v2 * -> v2 **
    v2.v2_foldl :: (** -> * -> **) -> ** -> v2 * -> **
    v2.v2_foldr :: (* -> ** -> **) -> ** -> v2 * -> **
    v2.v2_liftA2 :: (* -> ** -> ***) -> v2 * -> v2 ** -> v2 ***
    v2.v2_max :: ordI * -> v2 * -> v2 * -> v2 *
    v2.v2_min :: ordI * -> v2 * -> v2 * -> v2 *
    v2.v2_mod :: v2 int -> v2 int -> v2 int
    v2.v2_mul :: v2 int -> v2 int -> v2 int
    v2.v2_neg :: v2 int -> v2 int
    v2.v2_product :: v2 int -> int
    v2.v2_pure :: * -> v2 *
    v2.v2_quot :: v2 int -> v2 int -> v2 int
    v2.v2_rem :: v2 int -> v2 int -> v2 int
    v2.v2_sequence :: [v2 *] -> v2 ([*])
    v2.v2_signum :: v2 int -> v2 int
    v2.v2_sub :: v2 int -> v2 int -> v2 int
    v2.v2_sum :: v2 int -> int

### v3
 v3.am -- 3D vectors and associated operations

    v3.v3 * ::= v3.V3 * * *
    v3.V3 :: * -> * -> * -> v3 *
    v3.v3_abs :: v3 int -> v3 int
    v3.v3_add :: v3 int -> v3 int -> v3 int
    v3.v3_apply :: v3 (* -> **) -> v3 * -> v3 **
    v3.v3_bind :: v3 * -> (* -> v3 **) -> v3 **
    v3.v3_dist :: v3 int -> v3 int -> int
    v3.v3_div :: v3 int -> v3 int -> v3 int
    v3.v3_fmap :: (* -> **) -> v3 * -> v3 **
    v3.v3_foldl :: (** -> * -> **) -> ** -> v3 * -> **
    v3.v3_foldr :: (* -> ** -> **) -> ** -> v3 * -> **
    v3.v3_liftA2 :: (* -> ** -> ***) -> v3 * -> v3 ** -> v3 ***
    v3.v3_max :: ordI * -> v3 * -> v3 * -> v3 *
    v3.v3_min :: ordI * -> v3 * -> v3 * -> v3 *
    v3.v3_mod :: v3 int -> v3 int -> v3 int
    v3.v3_mul :: v3 int -> v3 int -> v3 int
    v3.v3_neg :: v3 int -> v3 int
    v3.v3_product :: v3 int -> int
    v3.v3_pure :: * -> v3 *
    v3.v3_quot :: v3 int -> v3 int -> v3 int
    v3.v3_rem :: v3 int -> v3 int -> v3 int
    v3.v3_sequence :: [v3 *] -> v3 ([*])
    v3.v3_signum :: v3 int -> v3 int
    v3.v3_sub :: v3 int -> v3 int -> v3 int
    v3.v3_sum :: v3 int -> int

### vector
vector.am -- immutable and mutable vectors, and the ST monad for sequencing in-place modification

    vector.mvector * ::= vector.MVector int word#
    vector.vector * ::= vector.Vector int word#
    vector.st * == state builtin.unit *
    vector.stRef * == mvector *
    (vector.!!) :: vector * -> int -> *
    (vector.//) :: vector * -> [(int, *)] -> vector *
    vector.MVector :: int -> word# -> mvector *
    vector.Vector :: int -> word# -> vector *
    vector.cmpvector :: ordI * -> vector * -> vector * -> ordering
    vector.modifySTRef :: stRef * -> (* -> *) -> st builtin.unit
    vector.newSTRef :: * -> st (stRef *)
    vector.readSTRef :: stRef * -> st *
    vector.runST :: st * -> *
    vector.runSTVector :: (mvector * -> st **) -> vector * -> vector *
    vector.safeIndex :: int -> int -> int
    vector.showvector :: showI * -> vector * -> string
    vector.v_all :: (* -> bool) -> vector * -> bool
    vector.v_any :: (* -> bool) -> vector * -> bool
    vector.v_append :: vector * -> vector * -> vector *
    vector.v_clone :: mvector * -> state ** (mvector *)
    vector.v_fill :: mvector * -> * -> state ** builtin.unit
    vector.v_filter :: (* -> bool) -> vector * -> vector *
    vector.v_find :: (* -> bool) -> vector * -> maybe *
    vector.v_first :: vector * -> *
    vector.v_fmap :: (* -> **) -> vector * -> vector **
    vector.v_fmapWithIndex :: (int -> * -> **) -> vector * -> vector **
    vector.v_foldl :: (** -> * -> **) -> ** -> vector * -> **
    vector.v_foldr :: (* -> ** -> **) -> ** -> vector * -> **
    vector.v_freeze :: mvector * -> state ** (vector *)
    vector.v_fromList :: [*] -> vector *
    vector.v_fromStream :: int -> stream * ** -> vector *
    vector.v_generate :: int -> (int -> *) -> vector *
    vector.v_index :: vector * -> int -> *
    vector.v_iterateN :: int -> (* -> *) -> * -> vector *
    vector.v_last :: vector * -> *
    vector.v_length :: vector * -> int
    vector.v_max :: ordI * -> vector * -> *
    vector.v_min :: ordI * -> vector * -> *
    vector.v_mlength :: mvector * -> int
    vector.v_modify :: mvector * -> (* -> *) -> int -> state ** builtin.unit
    vector.v_product :: vector int -> int
    vector.v_read :: mvector * -> int -> state ** *
    vector.v_rep :: int -> * -> vector *
    vector.v_replace :: vector * -> [(int, *)] -> vector *
    vector.v_search :: (* -> ordering) -> vector * -> maybe ((int, *))
    vector.v_singleton :: * -> vector *
    vector.v_sortBy :: ordI * -> vector * -> vector *
    vector.v_sum :: vector int -> int
    vector.v_thaw :: vector * -> mvector *
    vector.v_toList :: vector * -> [*]
    vector.v_toStream :: vector * -> stream * int
    vector.v_unsafeFreeze :: mvector * -> state ** (vector *)
    vector.v_unsafeIndex :: vector * -> int -> *
    vector.v_unsafeModify :: mvector * -> (* -> *) -> int -> state ** builtin.unit
    vector.v_unsafeRead :: mvector * -> int -> state ** *
    vector.v_unsafeReplace :: mvector * -> [(int, *)] -> state ** builtin.unit
    vector.v_unsafeThaw :: vector * -> mvector *
    vector.v_unsafeWrite :: mvector * -> int -> * -> state ** builtin.unit
    vector.v_write :: mvector * -> int -> * -> state ** builtin.unit
    vector.v_zipWith :: (* -> ** -> ***) -> vector * -> vector ** -> vector ***
    vector.writeSTRef :: stRef * -> * -> st builtin.unit

### zipper
zipper.am -- implementation of a list with a cursor

    zipper.zipper * ::= zipper.Zipper ([*]) ! ([*]) !
    zipper.Zipper :: [*] -> [*] -> zipper *
    zipper.z_begin :: zipper * -> zipper *
    zipper.z_beginp :: zipper * -> bool
    zipper.z_cursor :: zipper * -> *
    zipper.z_delete :: zipper * -> zipper *
    zipper.z_empty :: zipper *
    zipper.z_end :: zipper * -> zipper *
    zipper.z_endp :: zipper * -> bool
    zipper.z_fmap :: (* -> **) -> zipper * -> zipper **
    zipper.z_fold :: (** -> * -> **) -> ** -> zipper * -> **
    zipper.z_fromList :: [*] -> zipper *
    zipper.z_insert :: * -> zipper * -> zipper *
    zipper.z_left :: zipper * -> zipper *
    zipper.z_modify :: (* -> *) -> zipper * -> zipper *
    zipper.z_null :: zipper * -> bool
    zipper.z_pop :: zipper * -> zipper *
    zipper.z_push :: * -> zipper * -> zipper *
    zipper.z_right :: zipper * -> zipper *
    zipper.z_singleton :: * -> zipper *
    zipper.z_toList :: zipper * -> [*]

