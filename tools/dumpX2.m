|| dumpX2 -- pretty-print the .x2 file for a module, showing the type specs and definitions


%import <either>
%import <io>                    (>>=)/io_bind
%import <maybe>
%import <mirandaExtensions>
%import <state>
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

deserialize :: string -> io module
deserialize fn
    = mtimeFile fx >>= exists
      where
        fx         = withSuffix "x2" fn
        doDeserial = io_pure . st_evalState deserialModule

        exists tsm
            = error (fx ++ " required, but not found"), if tsm < 0
            = readFile fx >>= doDeserial,               otherwise

main :: io ()
main
    = getArgs >>= go
      where
        go [fn] = deserialize fn >>= showModule .> putStrLn
        go _    = error "usage: dumpX2 <file name>"
