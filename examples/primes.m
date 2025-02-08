%import <io>

primes :: [int]
primes = sieve [2 ..]
         where
           sieve (p : xs) = p : sieve [x | x <- xs; x $mod p ~= 0]

main :: io ()
main = primes |> take 100 |> showlist showint |> putStrLn
