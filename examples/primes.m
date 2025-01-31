|| primes.m -- generate primes the lazy recursive way

%import <io>
%import <mirandaExtensions>

primes
    = 2 : filter isPrime [3 ..]
      where
        isPrime n   = all (coPrime n) $ takeWhile (< n $div 2 + 1) primes
        coPrime n m = n $mod m ~= 0

main = primes |> take 100 |> showlist showint |> putStrLn
