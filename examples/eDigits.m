|| eDigits -- calculate and print the digits of "e"
||
|| shows the use of mutable vectors sequenced in the io monad
|| and the use of case expressions to ensure strictness

%import <io>           (>>=)/io_bind (>>)/io_right
%import <vector>


digits = 10_000

step :: mvector int -> int -> int -> io int
step mv 0 x = io_pure x
step mv i x
    = update >> v_unsafeRead mv i' >>= next
      where
        i'     = i - 1
        update = case x $mod i of xm -> v_unsafeWrite mv i xm   || ensure the updated value is strict (not a lazy thunk)
        next v = case 10 * v + x $div i of x' -> step mv i' x'  || ensure the next x value is strict

outer :: mvector int -> int -> int -> io ()
outer mv n x
    = showint x |> putStrLn,      if n <= 9
    = step mv (n - 2) x >>= next, otherwise
      where
        next x' = showint x' |> putStr >> outer mv (n - 1) x'

main :: io ()
main
    = v_rep digits 1 |> v_unsafeThaw |> go
      where
        go mv = v_unsafeWrite mv 0 0 >>
                v_unsafeWrite mv 1 2 >>
                outer mv digits 0
