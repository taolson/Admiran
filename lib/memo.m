|| memoization of a single-argument function with a state map
||

%export memoSt memo

%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <state>                 (>>=)/st_bind (<<)/st_left


|| a memoSt type uses a state consisting of an ordI instance for ordering the map's keys,
|| and a map from key (the memoized function's domain) to value (the memoized function's range)
memoSt * ** == state (ordI *, m_map * **) **

memo :: (* -> memoSt * **) -> * -> memoSt * **
memo f a
    = st_get >>= check
      where
        check (cmp, m)
            = fromMaybef (f a >>= update) st_pure (m_lookup cmp a m)
              where
                update fa = st_pure fa << st_modify (mapSnd (m_insert cmp a fa))

|| example of memoizing the fibonacci function
|| fib :: int -> memoSt int int
|| fib
||     = memo fib'
||       where
||         (<+>)  = st_liftA2 (+)
||         fib' 0 = st_pure 0
||         fib' 1 = st_pure 1
||         fib' n = fib (n - 1) <+> fib (n - 2)
|| 
|| %import <io>
|| main :: io ()
|| main = st_evalState (fib 80) (cmpint, m_empty) |> showint |> putStrLn
