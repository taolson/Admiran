|| bitSet -- representation of a set of small (< 64) natural numbers using an int


%export +


bitSet == int

bs_empty :: bitSet
bs_empty = 0

bs_null :: bitSet -> bool
bs_null n = n == 0

bs_singleton :: int -> bitSet
bs_singleton n = 1 .<<. n

|| a bitSet of size n with all bits set
bs_all :: int -> bitSet
bs_all n = (1 .<<. n) - 1

|| number of bits set in the bitSet (popcount)
bs_size :: bitSet -> int
bs_size
    = go 0
      where
        go s 0 = s
        go s n = case s + (n .&. 1) of s' -> go s' (n .>>. 1)

bs_member :: int -> bitSet -> bool
bs_member n bs = (bs .&. (1 .<<. n)) ~= 0

|| find index of first set bit
|| returns (-1) if bitSet is empty
bs_first :: bitSet -> int
bs_first
    = go (-1)
      where
        go i 0 = i
        go i n
            = case i + 1 of
                i' -> case n .>>. 1 of
                        n' -> case n .&. 1 == 0 of
                                False -> i'
                                True  -> go i' n'

|| find index of last bit set
|| returns (-1) if bitSet is empty
bs_last :: bitSet -> int
bs_last
    = go (-1)
      where
        go i 0 = i
        go i n
            = case i + 1 of
                i' -> case n .>>. 1 of
                        n' -> go i' n'

bs_insert, bs_delete :: int -> bitSet -> bitSet
bs_insert n bs = bs .|. (1 .<<. n)
bs_delete n bs = bs .&. complement (1 .<<. n)

bs_fromList :: [int] -> bitSet
bs_fromList = foldr bs_insert 0

bs_toList :: bitSet -> [int]
bs_toList
    = go 0
      where
        go i 0 = []
        go i n
            = case i + 1 of
                i' -> case n .>>. 1 of
                        n' -> case n .&. 1 == 0 of
                                False -> i : go i' n'
                                True  ->     go i' n'
