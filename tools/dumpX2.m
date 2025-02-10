|| dumpX2 -- pretty-print the .x2 file for a module, showing the type specs and definitions


%import <io>    (>>=)/io_bind (<$>)/io_fmap (>>)/io_right
%import <either>
%import <maybe>
%import <mirandaExtensions>
%import "../compiler/ast"
%import "../compiler/config"
%import "../compiler/dependency"
%import "../compiler/exception"
%import "../compiler/grammar"
%import "../compiler/module"
%import "../compiler/name"
%import "../compiler/parser"
%import "../compiler/predef"
%import "../compiler/serialize"
%import "../compiler/tokenizer"

|| turn an excpt * into an io *
io_excpt :: excpt * -> io *
io_excpt (Left errs)        = io_mapM_ (errStrLn . showException) errs  >> exit 1       || print the errors and exit
io_excpt (Right (r, warns)) = io_mapM_ (errStrLn . showException) warns >> io_pure r    || print the warnings and continue with the result

deserialize :: string -> io module
deserialize fn
    = mtimeFile fx >>= exists
      where
        fm         = withSuffix "m" fn
        fx         = withSuffix "x2" fn
        doDeserial = io_pure . fst . deserialModule

        exists tsm
            = io_excpt (ex_error err emptyLocInfo), if tsm < 0
            = readFile fx >>= doDeserial,           otherwise
              where
                err = Info $ fm ++ " required, but not found"

main :: io ()
main
    = getArgs >>= go
      where
        go [fn] = deserialize fn >>= showModule .> putStrLn
        go _    = error "usage: dumpX2 <file name>"
