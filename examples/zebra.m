|| zebra.m -- "who owns the zebra?" logic puzzle, solved using constraint logic


%import <bitSet>
%import <maybe>                 (<$>?)/mb_fmap (<|>?)/mb_alt
%import <mirandaExtensions>
%import <io>
%import <vector>


|| puzzle config

categories =
    [ ("colors",        ["blue", "green", "ivory", "red", "yellow"])
    , ("nationalities", ["englishman", "japanese", "norwegian", "spaniard", "ukrainian"])
    , ("drinks",        ["coffee", "milk", "orange juice", "tea", "water"])
    , ("pets",          ["dog", "fox", "horse", "snails", "zebra"])
    , ("smokes",        ["chesterfield", "kool", "lucky strike", "old gold", "parliment"])
    ]

givens =
    [ cPair             SameLoc         "englishman"    "red"                   || The Englishman lives in the red house.
    , cPair             SameLoc         "spaniard"      "dog"                   || The Spaniard owns a dog.
    , cPair             SameLoc         "coffee"        "green"                 || The person who drinks coffee lives in the green house.
    , cPair             SameLoc         "ukrainian"     "tea"                   || The Ukrainian drinks tea.
    , cPair             ImmRight        "green"         "ivory"                 || The green house is on the right of the ivory house.
    , cPair             SameLoc         "old gold"      "snails"                || The person who smokes Old Gold owns snails.
    , cPair             SameLoc         "kool"          "yellow"                || The person who smokes Kool lives in the yellow house.
    , cSingle           (LocIndex 2)    "milk"                                  || The person who drinks milk lives in the middle house.
    , cSingle           (LocIndex 0)    "norwegian"                             || The Norwegian lives in the first house (leftmost).
    , cPair             NextTo          "chesterfield"  "fox"                   || the person who smokes Chesterfield lives next to the person who owns a fox.
    , cPair             NextTo          "kool"          "horse"                 || The person who smokes Kool lives next to th eperson who owns a horse.
    , cPair             SameLoc         "lucky strike"  "orange juice"          || The person who smokes Lucky Strike drinks orange juice.
    , cPair             SameLoc         "japanese"      "parliment"             || the Japanese smokes Parliment.
    , cPair             NextTo          "norwegian"     "blue"                  || The Norwegian lives next to the blue house.
    ]


|| a catValue is an index pair into the categories vector, and the bit index in that category (bitSet)
|| category
catVal == (int, int)

|| find a catVal from a given string by searching the categories config for that string
findCatValue :: string -> catVal
findCatValue s
    = enumerate categories |> foldr checkCat Nothing |> fromMaybe err
      where
        err = error ("could not find the category and value indices for '" ++ s ++ "'")

        checkCat (i, (_, ns)) k
             = (pair i <$>? elemIndex cmpstring s ns) <|>? k

loc      == vector bitSet
solution == vector loc

showSolution :: solution -> string
showSolution s
    = v_toList s |> map showLoc |> lay
      where
        showLoc b = v_toList b |> enumerate |> showlist showCat

        showCat (i, bs)
            = hd vs,                  if #vs == 1
            = showlist showstring vs, otherwise
              where
                vs = [v | (j, v) <- categories ! i |> snd |> enumerate; bs_member j bs]

constraint ::=
    LocIndex    int    catVal |         || location at fixed index must have catVal
    SameLoc     catVal catVal |         || the same location must have both catVals
    ImmLeft     catVal catVal |         || location with catVal1 is immediately to the left of the location with catVal2
    ImmRight    catVal catVal |         || location with catVal1 is immediately to the right of the location with catVal2
    SomeLeft    catVal catVal |         || location with catVal1 is somewhere to the left of the location with catVal2
    SomeRight   catVal catVal |         || location with catVal1 is somewhere to the right of the location with catVal2
    NextTo      catVal catVal |         || location with catVal1 is right next to the location with catVal2 (either side) 
    OnEnd       catVal        |         || location with catVal is on one of the ends
    SomeBetween catVal catVal catVal    || location with catVal2 is somewhere between the location with catVal1 and the location with catVal3

|| constraint creation from strings

cSingle :: (catVal -> constraint) -> string -> constraint
cSingle c s = c $ findCatValue s

cPair :: (catVal -> catVal -> constraint) -> string -> string -> constraint
cPair c s1 s2 = c (findCatValue s1) (findCatValue s2)

cTriple :: (catVal -> catVal -> catVal -> constraint) -> string -> string -> string -> constraint
cTriple c s1 s2 s3 = c (findCatValue s1) (findCatValue s2) (findCatValue s3)


|| common location collections for implementing positional constraints

nLocs :: int
nLocs = hd categories |> snd |> length || all category values should have the same number as the number of locations

allLocs, allButFirstLoc, allButFirst2 :: [int]
allLocs        = [0 .. nLocs - 1]
allButFirstLoc = [1 .. nLocs - 1]
allButFirst2   = [2 .. nLocs - 1]

locsLeftOf, locsRightOf :: int -> [int]
locsLeftOf  i = [0 .. i - 1]
locsRightOf i = [i + 1 .. nLocs - 1]


|| constrain a solution, returning an empty list if the constraint cannot be met
constrain :: constraint -> solution -> [solution]

constrain (LocIndex bi (ci, v)) s
    = [],                       if ~bs_member v (s !! bi !! ci)
    = [v_mapWithIndex cnstr s], otherwise
      where
        cnstr i b
            = b // [(ci, bs_singleton v)],        if i == bi   || loc bi, category ci must be v
            = b // [(ci, bs_delete v (b !! ci))], otherwise    || loc i,  category ci cannot be v

constrain (SameLoc cv1 cv2) s
    = [s2 | i <- allLocs; s1 <- constrain (LocIndex i cv2) s;
                          s2 <- constrain (LocIndex i cv1) s1]

constrain (ImmLeft cv1 cv2) s
    = [s2 | i <- allButFirstLoc; s1 <- constrain (LocIndex i cv2) s;
                                 s2 <- constrain (LocIndex (i - 1) cv1) s1]

constrain (ImmRight cv1 cv2) s
    = constrain (ImmLeft cv2 cv1) s

constrain (SomeLeft cv1 cv2) s
    = [s2 | i <- allButFirstLoc; s1 <- constrain (LocIndex i cv2) s;
            j <- locsLeftOf i;  s2 <- constrain (LocIndex j cv1) s1]

constrain (SomeRight cv1 cv2) s
    = constrain (SomeLeft cv2 cv1) s

constrain (NextTo cv1 cv2) s
    = constrain (ImmLeft cv1 cv2) s ++ constrain (ImmLeft cv2 cv1) s

constrain (OnEnd cv) s
    = constrain (LocIndex 0 cv) s ++ constrain (LocIndex (nLocs - 1) cv) s

constrain (SomeBetween cv1 cv2 cv3) s
    = [s3 | i <- allButFirst2; s1 <- constrain (LocIndex i cv3) s;
            j <- locsLeftOf i; s2 <- constrain (LocIndex j cv2) s1;
            k <- locsLeftOf j; s3 <- constrain (LocIndex k cv1) s2]


|| consecutively apply the given constraints to an initial maximal solution
solve :: [constraint] -> [solution]
solve cs
    = foldl go [init] cs
      where
        init    = bs_all nLocs |> v_rep (#categories) |> v_rep nLocs
        go ss c = [s1 | s <- ss; s1 <- constrain c s]


main :: io ()
main
    = solve givens |> go
      where
        go []  = putStrLn "no solutions found!"
        go ss  = io_mapM_ (showSolution .> putStrLn) ss
