|| sloc.m -- report Source Lines Of Code for files specified on the command line (without counting pure comment lines)


%import <io>                    (>>=)/io_bind (>>)/io_right
%import <maybe>
%import <mirandaExtensions>


|| report whether line is a code line or not (any that isn't blank or only a comment)
isCodeLine :: string -> bool
isCodeLine
    = words .> viewL .> fromMaybef False (fst .> isPrefixOf cmpchar "||" .> not)

|| right-align a string in a given field size by padding it with spaces
rightAlign n s = rep (n - #s) ' ' ++ s ++ " "

|| process a file, reporting its sloc and returning a running tally of the total sloc
process :: int -> string -> io int
process tot fn
    = readFile fn >>= lines .> filter isCodeLine .> length .> report
      where
        report n
              || the following ensures that we have finished with the file (including the implicit close of it), by
              || forcing the running total to be strict.  If we didn't, the files would be lazily processed, causing
              || a bunch of files to be opened and processed in parallel, then closed, resulting in a "too many open files"
              || error if we try to do a total report on too many.
            = case tot + n of tot' -> showint n |> rightAlign 5 |> (++ fn) |> putStrLn >> io_pure tot'

main :: io ()
main = getArgs >>= io_foldM process 0 >>= reportTotal
       where
         reportTotal = showint .> rightAlign 5 .> ("\n" ++) .> (++ "total") .> putStrLn
