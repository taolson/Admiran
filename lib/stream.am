||  stream.m -- implementation of streams from the paper:
||
||  "Stream Fusion -- From Lists to Streams to Nothing at All"
||
|| This module can be used explicitly, but can also be used in the compiler "inline" pass,
|| which builds a substitution map from standard list functions to the stream equivalents.
|| To handle this, the names have a structure:
||     - names of internal stream versions end in "S"
||     - names of backwards-compatible list versions (wrapped in toStream/fromStream) are the same as the list versions,
||       with a "_" (or "@", for symbols) at the end
|| this allows inlining to automatically determine the substitution map
||
|| note: there is a circular dependency between stdlib and stream; stream depends upon stdlib functions, but some
||       stdlib functions should be replaced by stream equivalents.  To handle this, stdlib currently needs to be
||       manually rebuilt anytime stream is rebuilt.
||
|| note: fusion is very sensitive to where the toStream/fromStream calls are; they won't collapse if they are not in
||       just the right place.  Currently this means that the compatibility functions (e.g. map_, foldr_) cannot be
||        written using the (.) compose operator, but instead have to be written explicitly with nested function calls.

%export + -uncurry      || uncurry defined in mirandaExtensions, but defined and used internally, here, to avoid dependency


|| note: in the original definition, the second type var is existential, restricting the operations that
|| a stream operation can perform.  Since Miranda has no existential types, it is made explicit.
|| the first type var is the type of the stream element, while the second var is the type of the stream
|| state
stream * ** ::= Stream (** -> step * **) **

step * ** ::= Done | Skip ** | Yield * **

|| internal implementations of "either" and "maybe" data types, so that this module doesn't have a dependency
|| on the actual ones
seither * ** ::= Sleft * | Sright **
smaybe *     ::= Snothing | Sjust *


|| internal stream versions of stdlib and mirandaExtension list functions

|| ord instance for stream
cmpstream :: ordI * -> ordI (stream * **)
cmpstream cmp' (Stream nexta sa) (Stream nextb sb)
    = goA sa sb
      where
        goA sa sb
            = case nexta sa of
                Done        -> goB sa  sb Snothing
                Skip sa'    -> goA sa' sb
                Yield a sa' -> goB sa' sb (Sjust a)

        goB sa sb ma
            = case nextb sb of
                Done        -> case ma of
                                 Snothing -> EQ
                                  _       -> GT
                Skip sb'    -> goB sa sb' ma
                Yield b sb' -> case ma of
                                 Snothing -> LT
                                 Sjust a  -> case cmp' a b of
                                               EQ -> goA sa sb'
                                               o  -> o

readByteStreamS :: word# -> stream char word#
readByteStreamS p#
    = Stream next p#
      where
        || readByteStream# returns (-1)# at end of stream
        next p# = case readByteStream# p# of
                    c# -> case c# $cmp# 0# of
                            1# -> Done
                            _  -> Yield (C# c#) p#

rangeByS :: int -> int -> int -> stream int int
rangeByS step lo hi
    = Stream nextInc lo, if step > 0
    = Stream nextDec lo, otherwise
      where
        nextInc n = Done, if n > hi
                  = Yield n (n + step), otherwise

        nextDec n = Done, if n < hi
                  = Yield n (n + step), otherwise

rangeS :: int -> int -> stream int int
rangeS n = rangeByS 1 n

rangeByFromS :: int -> int -> stream int int
rangeByFromS step n
    = Stream next n
      where
        next n = Yield n (n + step)

rangeFromS :: int -> stream int int
rangeFromS n = rangeByFromS 1 n

iterateS :: (* -> *) -> * -> stream * *
iterateS f x
    = Stream next x
      where
        next x = case f x of x' -> Yield x x'

tlS :: stream * ** -> stream * (bool, **)
tlS (Stream next s)
    = Stream next' (False, s)
      where
        next' (False, s)
            = case next s of
                Done       -> error "tlS []"
                Skip s'    -> Skip (False, s')
                Yield _ s' -> Skip (True, s')
        next' (True, s)
            = case next s of
                Done       -> Done
                Skip s'    -> Skip (True, s')
                Yield x s' -> Yield x (True, s')

mapS :: (* -> ***) -> stream * ** -> stream *** **
mapS f (Stream next s)
    = Stream next' s
      where
        next' s = case next s of
                    Done       -> Done
                    Skip s'    -> Skip s'
                    Yield x s' -> Yield (f x) s'

filterS :: (* -> bool) -> stream * ** -> stream * **
filterS p (Stream next s)
    = Stream next' s
      where
        next' s = case next s of
                    Done       -> Done
                    Skip s'    -> Skip s'
                    Yield x s' -> case p x of
                                    False -> Skip s'
                                    True  -> Yield x s'

foldrS :: (* -> *** -> ***) -> *** -> stream * ** -> ***
foldrS f z (Stream next s)
    = go s
      where
        go s = case next s of
                 Done       -> z
                 Skip s'    -> go s'
                 Yield x s' -> f x (go s')

foldlS :: (* -> *** -> *) -> * -> stream *** ** -> *
foldlS f z (Stream next s)
    = go z s
      where
        go z s = case next s of
                   Done       -> z
                   Skip s'    -> go z s'
                   Yield x s' -> case f z x of z' -> (go z' s')

lengthS :: stream * ** -> int
lengthS s = foldlS inc 0 s where inc n _ = n + 1

appendS :: stream * ** -> stream * *** -> stream * (seither ** ***)
appendS (Stream nexta sa) (Stream nextb sb)
    = Stream next (Sleft sa)
      where
        next (Sleft s)  = case nexta s of
                           Done       -> Skip (Sright sb)
                           Skip s'    -> Skip (Sleft s')
                           Yield x s' -> Yield x (Sleft s')

        next (Sright s) = case nextb s of
                           Done       -> Done
                           Skip s'    -> Skip (Sright s')
                           Yield x s' -> Yield x (Sright s') 

zip2S :: stream * ** -> stream *** **** -> stream (*, ***) (**, ****, smaybe *)
zip2S (Stream nexta sa) (Stream nextb sb)
    = Stream next (sa, sb, Snothing)
      where
        next (sa, sb, Snothing)      || don't have an element from the first stream, yet
            = case nexta sa of
                Done        -> Done
                Skip sa'    -> Skip (sa', sb, Snothing)
                Yield a sa' -> Skip (sa', sb, Sjust a)

        next (sa, sb, Sjust a)
            = case nextb sb of
                Done        -> Done
                Skip sb'    -> Skip (sa, sb', Sjust a)
                Yield b sb' -> Yield (a, b) (sa, sb', Snothing)

zipWithS :: (* -> ** -> ***) -> stream * **** -> stream ** ***** -> stream *** (****, *****, smaybe *)
zipWithS f sa sb = mapS (uncurry f) (zip2S sa sb)

concatMapS :: (* -> stream ** ***) -> stream * **** -> stream ** ((****, smaybe (stream ** ***)))
concatMapS f (Stream nexta sa)
    = Stream next (sa, Snothing)
      where
        next (sa, Snothing)
            = case nexta sa of
                Done        -> Done
                Skip sa'    -> Skip (sa', Snothing)
                Yield a sa' -> Skip (sa', Sjust (f a))

        next (sa, Sjust (Stream nextb sb))
            = case nextb sb of
                Done        -> Skip (sa, Snothing)
                Skip sb'    -> Skip (sa, Sjust (Stream nextb sb'))
                Yield b sb' -> Yield b (sa, Sjust (Stream nextb sb'))

takeS :: int -> stream * ** -> stream * (int, **)
takeS n (Stream next s)
    = Stream next' (n, s)
      where
        next' (n, s) = case next s of
                         Done       -> Done
                         Skip s'    -> Skip (n, s')
                         Yield x s' -> case n > 0 of
                                         False -> Done
                                         True  -> Yield x (n - 1, s')

dropS :: int -> stream * ** -> stream * (int, **)
dropS n (Stream next s)
    = Stream next' (n, s)
      where
        next' (n, s) = case next s of
                         Done       -> Done
                         Skip s'    -> Skip (n, s')
                         Yield x s' -> case n > 0 of
                                         False -> Yield x (n, s')
                                         True  -> Skip (n - 1, s')

takeWhileS :: (* -> bool) -> stream * ** -> stream * **
takeWhileS p (Stream next s)
    = Stream next' s
      where
        next' s = case next s of
                    Done       -> Done
                    Skip s'    -> Skip s'
                    Yield x s' -> case p x of
                                    False -> Done
                                    True  -> Yield x s'

dropWhileS :: (* -> bool) -> stream * ** -> stream * (bool, **)
dropWhileS p (Stream next s)
    = Stream next' (True, s)
      where
        next' (b, s) = case next s of
                         Done       -> Done
                         Skip s'    -> Skip (b, s')
                         Yield x s' -> case b & p x of
                                         False -> Yield x (False, s')
                                         True  -> Skip (True, s')

interleaveS :: stream * ** -> stream * *** -> stream * (bool, **, ***)
interleaveS (Stream nexta sa) (Stream nextb sb)
    = Stream next (False, sa, sb)
      where
        next (False, sa, sb)
            = case nexta sa of
                Done        -> Done
                Skip sa'    -> Skip (False, sa', sb)
                Yield a sa' -> Yield a (True, sa', sb)

        next (True, sa, sb)
            = case nextb sb of
                Done        -> Done
                Skip sb'    -> Skip (True, sa, sb')
                Yield b sb' -> Yield b (False, sa, sb')

linesS :: stream char * -> stream [char] (smaybe [char])
linesS (Stream next st)
    = Stream next' (Sjust [])
      where
        next' Snothing = Done
        next' (Sjust xs)
            = case next st of
                Done      -> Yield (reverse xs) Snothing
                Skip _    -> Skip (Sjust xs)
                Yield c _ -> case c ==. '\n' of
                               False -> Skip $ Sjust (c : xs)
                               True  -> Yield (reverse xs) (Sjust [])

wordsS :: stream char * -> stream [char] (smaybe [char])
wordsS (Stream next st)
    = Stream next' (Sjust [])
      where
        isSpace c = c ==. ' ' \/ c ==. '\t' \/ c ==. '\n'

        next' Snothing = Done
        next' (Sjust xs)
            = case next st of
                Done      -> Yield (reverse xs) Snothing
                Skip _    -> Skip (Sjust xs)
                Yield c _ -> case isSpace c of
                               False -> Skip $ Sjust (c : xs)
                               True  -> Yield (reverse xs) (Sjust [])


|| backwards-compatible re-implementations of stdlib and mirandaExtension list functions,
|| wrapping the internal stream functions with fromStream/toStream

uncurry :: (* -> ** -> ***) -> (*, **) -> ***
uncurry f = go where go (a, b) = f a b

|| convert list to stream
toStream :: [*] -> stream * [*]
toStream xs
    = Stream next xs
      where
        next []       = Done
        next (x : xs) = Yield x xs

|| convert stream to list
fromStream :: stream * ** -> [*]
fromStream (Stream next s)
    = unfold s
      where
        unfold s = case next s of
                     Done       -> []
                     Skip s'    -> unfold s'
                     Yield x s' -> x : unfold s'

|| note: using readByteStream' doesn't allow toStream/fromStream collapsing of literal [char]s,
|| because we don't inline complex exprs that have more than 1 use or that cross a DefFn boundary,
|| to limit the possibility of duplicating work.
readByteStream_ :: word# -> [char]
readByteStream_ w = fromStream (readByteStreamS w)

rangeBy_ :: int -> int -> int -> [int]
rangeBy_ step lo hi = fromStream (rangeByS step lo hi)

range_ :: int -> int -> [int]
range_ lo hi = fromStream (rangeByS 1 lo hi)

rangeByFrom_ :: int -> int -> [int]
rangeByFrom_ step n = fromStream (rangeByFromS step n)

rangeFrom_ :: int -> [int]
rangeFrom_ n = fromStream (rangeByFromS 1 n)

iterate_ :: (* -> *) -> * -> [*]
iterate_ f x = fromStream (iterateS f x)

tl_ :: [*] -> [*]
tl_ xs = fromStream (tlS (toStream xs))

map_ :: (* -> **) -> [*] -> [**]
map_ f xs = fromStream (mapS f (toStream xs))

filter_ :: (* -> bool) -> [*] -> [*]
filter_ f xs = fromStream (filterS f (toStream xs))

foldr_ :: (* -> ** -> **) -> ** -> [*] -> **
foldr_ f z xs = (foldrS f z (toStream xs))

foldl_ :: (* -> ** -> *) -> * -> [**] -> *
foldl_ f z xs = foldlS f z (toStream xs)

length_, (#@) :: [*] -> int
length_ xs = lengthS (toStream xs)
(#@)       = length_

(++@) :: [*] -> [*] -> [*]
(++@) xs ys = fromStream (appendS (toStream xs) (toStream ys))

|| a copy of stdlib.(.) that allows inlining of (.) during the streamSubst pass
(.@) :: (** -> ***) -> (* -> **) -> * -> ***
(.@) f g = go where go x = f (g x)

zip2_ :: [*] -> [**] -> [(*, **)]
zip2_ xs ys = fromStream (zip2S (toStream xs) (toStream ys))

zipWith_ :: (* -> ** -> ***) -> [*] -> [**] -> [***]
zipWith_ f xs ys = fromStream (zipWithS f (toStream xs) (toStream ys))

concatMap_ :: (* -> [**]) -> [*] -> [**]
concatMap_ f xs = fromStream (concatMapS (toStream . f) (toStream xs))

take_ :: int -> [*] -> [*]
take_ n xs = fromStream (takeS n (toStream xs))

drop_ :: int -> [*] -> [*]
drop_ n xs = fromStream (dropS n (toStream xs))

takeWhile_ :: (* -> bool) -> [*] -> [*]
takeWhile_ p xs = fromStream (takeWhileS p (toStream xs))

dropWhile_ :: (* -> bool) -> [*] -> [*]
dropWhile_ p xs = fromStream (dropWhileS p (toStream xs))

interleave_ :: [*] -> [*] -> [*]
interleave_ xs ys = fromStream (interleaveS (toStream xs) (toStream ys))

lines_ :: [char] -> [[char]]
lines_ s = fromStream (linesS (toStream s))

words_ :: [char] -> [[char]]
words_ s = fromStream (wordsS (toStream s))
