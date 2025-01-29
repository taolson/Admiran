|| io.m -- the IO monad for sequencing access to the outside world


%export + io_bind io_pure io_kbind io_mapM io_mapM_ io_foldM io_fmap io_apply io_liftA2 io_left io_right io_bind2

%import <state> io_bind/st_bind io_pure/st_pure io_kbind/st_kbind io_mapM/st_mapM io_mapM_/st_mapM_ io_foldM/st_foldM
                io_fmap/st_fmap io_apply/st_apply io_liftA2/st_liftA2 io_left/st_left io_right/st_right io_bind2/st_bind2

|| a wrapper on a byteStream# (which is just a word# that represents the raw heapAddr of an allocated byteStream#)
|| which allows for top-level handle thunks like stdin
handle ::= Handle word#

io * == state () *

|| Note: the following routines are written with an explicit state "world" argument, rather than partially-
|| applied with an implicit state, because they don't rely upon the sequencing inherent in the state
|| operations, but rather have a chain of case statements which handle strict evaluation of primitive
|| operations.  However, we need to ensure that the state "world" is available before performing the
|| case operations, so world is an explicit argument which is simply passed in, then returned at the end.
|| Also, since the compiler inliner explicitly avoids inlining any function in this module, they
|| don't benefit from being written in partially-applied form to aid with inlining.

|| read a character from a handle, re-filling the buffer from the file when it is empty
hGetChar :: handle -> io char
hGetChar (Handle fs#)
    = io_pure $ restart fs#
      where
        restart fs#
            = case readByteStream# fs# of
                c# -> case c# $cmp# 0# of
                        1# -> case readFile# fs# of             || buffer empty; reload from file
                                r# -> case r# $cmp# 0# of       || check for end of file (0 bytes read)
                                        0# -> C# c#             || end of file; return (-1)
                                        _  -> restart fs#       || filled buffer; restart getLine
                        _  -> C# c#

hGetLine :: handle -> io string
hGetLine (Handle fs#)
    = io_pure $ restart fs#
      where
        restart fs#
            =  case readByteStream# fs# of
                c# -> case c# $cmp# 0# of
                        1# -> case readFile# fs# of             || buffer empty; reload from file
                                r# -> case r# $cmp# 0# of       || check for end of file (0 bytes read)
                                        0# -> []                || end of file; return []
                                        _  -> restart fs#       || filled buffer; restart getLine
                        _  -> case c# $cmp# '\n'# of            || check for newline character
                                0# -> []                        || got newline; return []
                                _  -> C# c# : restart fs#       || non-newline; return it with the lazy remaining line

hGetContents :: handle -> io string
hGetContents h = readFileStream h

hPutChar :: handle -> char -> io ()
hPutChar h c = writeFileStream h [c]

hPutStr :: handle -> string -> io ()
hPutStr h s = writeFileStream h s


|| read from an handle, re-filling the buffer from the file when it is empty
|| readFileStream is strict for each buffer read, but reloading the buffer from the file is lazy
readFileStream :: handle -> io string
readFileStream (Handle fs#)
    = io_pure $ restart fs#
      where
        restart fs#                                             || Note: fs# is a dummy argument to prevent restart from being a memoized thunk
            = case readByteStream fs# of                        || fs# never changes, but the contents of fs# change
                [] -> case readFile# fs# of                     || buffer empty; reload from file
                        r# -> case r# $cmp# 0# of               || check for end of file (0 bytes read)
                                0# -> case closeFile# fs# of    || close file (runtime ignores close requests for stdin/out/err)
                                        _ -> []                 || and return end of stream
                                _  -> restart fs#               || filled buffer; restart readFileStream
                s  -> s ++ restart fs#                          || strictly read as much of the buffer as possible, with lazy refill of the buffer
        
|| strict write to an handle, writing the buffer out to the file when it becomes full
writeFileStream :: handle -> string -> io ()
writeFileStream (Handle fs#) s
    = restart s
      where
        restart s world
            = case writeByteStream fs# s of
                || write complete; write any remaining bytes in byteStream out to the file and close it
                [] -> case writeFile# fs# of
                        _ -> case closeFile# fs# of
                               _ -> ((), world)         || runtime ignores close requests for stdin/out/err

                || buffer full; write the buffer out to the file, then continue with the unwritten portion of the string
                s' -> case writeFile# fs# of _ -> restart s' world

|| read the contents of a file named by fn as a string
readFile :: string -> io string
readFile fn world
    = case allocByteStream# 256# of                     || alloc an empty byteStream buffer of size 256 qwords (2048 bytes)
        s# -> case writeByteStream s# fn of             || write the fn (an arbitrary Miranda string) to the byteStream
                _ -> case openFileRead# s# of           || openFile returns the fileStream with the contents
                       fs# -> readFileStream (Handle fs#) world

|| write to an existing file or create a new one
writeFile :: string -> string -> io ()
writeFile fn s world
    = case allocByteStream# 256# of                     || alloc an empty byteStream buffer of size 256 qwords (2048 bytes)
        s# -> case writeByteStream s# fn of             || write the fn (an arbitrary Miranda string) to the byteStream
                _ -> case openFileWrite# s# of          || openFile returns the file buffer byteStream to write into
                       fs# -> writeFileStream (Handle fs#) s world

|| append to an existing file or create a new one
appendFile :: string -> string -> io ()
appendFile fn s world
    = case allocByteStream# 256# of                     || alloc an empty byteStream buffer of size 256 qwords (2048 bytes)
        s# -> case writeByteStream s# fn of             || write the fn (an arbitrary Miranda string) to the byteStream
                _ -> case openFileAppend# s# of         || openFile returns the file buffer byteStream to write into
                       fs# -> writeFileStream (Handle fs#) s world

|| get the modification timestamp for a file (returns -1 for non-existent file)
mtimeFile :: string -> io int
mtimeFile fn world
    = case allocByteStream# 256# of
        s# -> case writeByteStream s# fn of
                _ -> case mtimeFile# s# of
                       ts# -> (I# ts#, world)

|| execute a shell command and return the result
systemCmd :: string -> io int
systemCmd cmd world
    = case allocByteStream# 256# of
        s# -> case writeByteStream s# cmd of
                _ -> case systemCmd# s# of
                       r# -> (I# r#, world)


|| operations on stdin, stdout, stderr

stdin, stdout, stderr :: handle
stdin  = case allocFileStream# 0# 32# of fs# -> Handle fs#
stdout = case allocFileStream# 1# 32# of fs# -> Handle fs#
stderr = case allocFileStream# 2# 32# of fs# -> Handle fs#

|| put a string to stdout
putStr :: string -> io ()
putStr s = writeFileStream stdout s

|| put a string with a final newline character to stdout
putStrLn :: string -> io ()
putStrLn s = putStr (s ++ "\n")

|| put a string to stderr
errStr :: string -> io ()
errStr s = writeFileStream stderr s

|| put a string with a final newline character to stderr
errStrLn :: string -> io ()
errStrLn s = errStr (s ++ "\n")

|| the following get functions for stdin are written with an explicit "world" argument for io, because
|| otherwise they would be CAFs that get updated to a fixed result after the first call

|| get a char from stdin
getChar :: io char
getChar world = hGetChar stdin world

|| get a line from stdin
getLine :: io string
getLine world = hGetLine stdin world

|| get the entire contents of stdin as a string
|| getContents is strict for each buffer read, but reloading the buffer from stdin is lazy
getContents :: io string
getContents world = hGetContents stdin world

|| get the commandLine arguments
|| note: this is a CAF instead of a function, which is ok because the getArgs value
|| is constant, so it can be updated to its computed value
getArgs :: io [string]
getArgs
    = case getArg# 0# of                                || getArg 0# returns the number of args in the argv vector (argc)
        n# -> io_pure $ map getArg [1 .. I# n#]         || convert each arg lazily into a Miranda list of byteStreams
      where
        getArg (I# n#) = case getArg# n# of s# -> readByteStream s#
