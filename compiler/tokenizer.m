|| tokenizer.m -- lexical syntax tokenizer
||    this version uses continuation-passing style (CPS) to pass
||    character streams and locations between tokenizers; this saves
||    some extra constructor-building over using tuples to build up
||    and tear down arguments and return values.  However, it does
||    make some things more difficult -- in particular the tokenizeString
||    tokenizer has to use a reverse on a built-up string rather than
||    building it up on the stack.
||
|| supports the following extensions to Miranda's lexical grammar:
||
||    identifiers can be qualified with a module name, e.g. stdlib.sum
||    primitive (unboxed) integers and chars specified with a '#' suffix, e.g. 123# or 'x'#
||    decimal, octal (leading 0), hexadecimal (0x) or binary (0b) supported for ints
||    underscores (_) can be used when writing integers as grouping separators, e.g. 123_456_789 or 0x1234_abcd
||    $ is tokenized as an operator if it is not immediately followed by a letter ($ident is the infix form of ident)

%export token cmptoken showToken tokloc cmptokloc showtokloc tokenize isTerror isTend

%import <maybe>
%import <mirandaExtensions>
%import "name"


token ::=
    Tident     name     |       || identifier
    Tsymbol    name     |       || symbol (binary operator)
    TlitInt    int      |       || literal integer
    TlitChar   char     |       || literal character e.g. 'x' 
    TlitString string   |       || literal string e.g. "this is a test"
    TprimInt   int      |       || primitive int literal (int#)
    TprimChar  char     |       || primitive char literal (char#)
    Tchar      char     |       || character not in symbols e.g. brace, paren
    Toffside            |       || token representing an outdent from the current indentation level
    Tend                |       || end of input
    Terror     string           || error encountered during lexical tokenize

isTerror, isTend :: token -> bool
isTerror (Terror err) = True
isTerror t            = False

isTend Tend = True
isTend t    = False

showToken :: token -> string
showToken (Tident n)     = showName n
showToken (Tsymbol n)    = showName n
showToken (TlitInt n)    = showint n
showToken (TlitChar c)   = showchar c
showToken (TlitString s) = showstring s
showToken (TprimInt n)   = showint n ++ "#"
showToken (TprimChar c)  = showchar c ++ "#"
showToken (Tchar c)      = showchar c
showToken Toffside       = "OFFSIDE"
showToken Tend           = "END"
showToken (Terror s)     = "ERROR " ++ s

tokloc == (token, int, int)                   || token, line#, col#


tokenize :: string -> [tokloc]
tokenize = tokenizePos 1 1

tokError :: string -> int -> int -> [tokloc]
tokError err il ic = [(Terror err, il, ic), (Tend, il, ic)]


tokenizer == int -> int -> string -> [tokloc]  || line#, col#, input

tokenizePos :: tokenizer
tokenizePos il ic [] = [(Tend, il, ic)]
tokenizePos il ic cs
    = tokenizePos                     il  ict cs', if c ==. '\t'  || account for tab's effect on column number by assuming tab stops every 8 chars
    = tokenizePos                     il  ic' cs', if c ==. ' '
    = tokenizePos                     il' 1   cs', if c ==. '\n'
    = tokenizeComment                 il  ic' cs', if c ==. '|' & ~null cs' & c' ==. '|'
    = tokenizeIdent                   il  ic  cs,  if letter c \/ c ==. '_'
    = tokenizeInfixIdent              il  ic' cs', if c ==. '$' & ~null cs' & letter c'
    = tokenizeInfixSymbol             il  ic  cs,  if isSymbol c
    = tokenizeLitInt                  il  ic  cs,  if digit c
    = tokenizeLitChar                 il  ic  cs,  if c ==. '\''
    = tokenizeLitString               il  ic  cs,  if c ==. '\"'
    = (Tchar c, il, ic) : tokenizePos il  ic' cs', otherwise
      where
        c  : cs'  = cs
        c'        = hd cs'
        ic'       = ic + 1
        ict       = (ic $div 8 + 1) * 8
        il'       = il + 1

isIdent, isSymbol :: char -> bool
isIdent  c = letter c \/ digit c \/ c ==. '_' \/ c ==. '\''
isSymbol c = member cmpchar "!#$%&*-+./:<=>?@\\^~|" c

tokenizeComment :: tokenizer
tokenizeComment il ic cs
    = tokenizePos il ic' cs'
      where
        (is, cs') = span (~=. '\n') cs
        ic'       = ic + #is

tokenizeIdent :: tokenizer
tokenizeIdent
    = tokenizeQualified cont
      where
        cont t il ic cs = t : tokenizePos il ic cs

tokenizeInfixIdent :: tokenizer
tokenizeInfixIdent il ic cs
    = tokenizeQualified cont il ic cs
      where
        cont (Tident n, il, ic) il' ic' cs' = (Tsymbol n,  il, ic) : tokenizePos il' ic' cs'
        cont t                  il' ic' cs' = tokError "expected an infix identifier" il ic

|| tokenize an optionally qualified identifier or symbol
tokenizeQualified :: (tokloc -> tokenizer) -> tokenizer
tokenizeQualified k il ic cs
    = tokenizeStr isIdent mkIdent il ic cs
      where
        mkIdent s il1 ic1 cs1
            = tokError "expected an identifier" il1 ic1,         if null s
            = k (Tident (Unqualified s), il, ic) il1 ic1 cs1,    if null cs1 \/ c1 ~=. '.' \/ null cs2   || unqualified identifier
            = tokenizeStr isIdent  (mkQual Tident)  il1 ic2 cs2, if letter c2 \/ c2 ==. '_'              || qualified identifier
            = tokenizeStr isSymbol (mkQual Tsymbol) il1 ic2 cs2, if isSymbol c2                          || qualified symbol
            = tokError "illegal qualified name" il ic,           otherwise
              where
                c1 : cs2                  = cs1
                c2                        = hd cs2
                ic2                       = ic1 + 1
                mkQual con s' il' ic' cs' = k (con (Unresolved s s'), il, ic) il' ic' cs'

tokenizeInfixSymbol :: tokenizer
tokenizeInfixSymbol il ic cs
    = tokenizeStr isSymbol mkSym il ic cs
      where
        mkSym s il' ic' cs' = (Tsymbol (Unqualified s), il, ic) : tokenizePos il' ic' cs'

tokenizeStr :: (char -> bool) -> (string -> tokenizer) -> tokenizer
tokenizeStr tst k il ic cs
    = k is il ic1 cs1,           if null cs1 \/ c1 ~=. '#'
    = k (is ++ [c1]) il ic2 cs2, otherwise      || allow trailing '#' in a symbol (used to denote unboxed types/values)
      where
        (is, cs1) = span tst cs
        c1 : cs2  = cs1
        ic1       = ic + #is
        ic2       = ic1 + 1

tokenizeLitInt :: tokenizer
tokenizeLitInt il ic cs
    = tokenizeInt litInt il ic cs
      where
        litInt n il1 ic1 cs1 = (TlitInt n, il, ic)  : tokenizePos il1 ic1 cs1, if null cs1 \/ c1 ~=. '#'
                             = (TprimInt n, il, ic) : tokenizePos il1 ic2 cs2, otherwise
                               where
                                 c1 : cs2 = cs1
                                 ic2      = ic1 + 1

tokenizeInt :: (int -> tokenizer) -> tokenizer
tokenizeInt k il ic cs
    = tokenizeRadix radix k il ic' cs'
      where
        c1 : cs1 = cs
        c2 : cs2 = cs1
        radix    = 10,     if c1 ~=. '0'
                 = 16,     if ~null cs1 & (c2 ==. 'X' \/ c2 ==. 'x')
                 = 2,      if ~null cs1 & (c2 ==. 'B' \/ c2 ==. 'b')
                 = 8,      otherwise
        ic'      = ic,     if radix == 10 \/ radix == 8
                 = ic + 2, otherwise
        cs'      = cs,     if radix == 10 \/ radix == 8
                 = cs2,    otherwise

tokenizeRadix :: int -> (int -> tokenizer) -> tokenizer
tokenizeRadix radix k il ic cs
    = k n il ic' cs'
      where
        (is, cs')      = span radixTest cs
        ic'            = ic + #is
        n              = foldl addDigit 0 is
        radixTest c    = c ==. '_' \/ 0 <= radixCode c < radix
        addDigit s '_' = s                                      || allow for embedded _ separator
        addDigit s c   = s * radix + radixCode c
        radixCode c    = code c - code '0',      if '0' <=. c <=. '9'
                       = code c - code 'A' + 10, if 'A' <=. c <=. 'F'
                       = code c - code 'a' + 10, if 'a' <=. c <=. 'f'
                       = -1,                     otherwise

tokenizeLitChar :: tokenizer
tokenizeLitChar il ic cs
    = tokenizeChar checkTerm il ic' cs'
      where
        cs' = tl cs
        ic' = ic + 1
        checkTerm c il1 ic1 cs1
            = tokError "expected '" il1 ic1,                   if null cs1
            = tokError "illegal character literal" il1 ic1,    if c1 ~=. '\''
            = (TlitChar c, il, ic)  : tokenizePos il1 ic2 cs2, if null cs2 \/ c2 ~=. '#'
            = (TprimChar c, il, ic) : tokenizePos il1 ic3 cs3, otherwise
              where
                c1 : cs2 = cs1
                ic2      = ic1 + 1
                c2 : cs3 = cs2
                ic3      = ic2 + 1

tokenizeChar :: (char -> tokenizer) -> tokenizer
tokenizeChar k il ic [] = tokError "expected '" il ic
tokenizeChar k il ic cs
    = tokenizeEscapeChar k il ic' cs', if c ==. '\\'
    = k c il ic' cs',                  otherwise
      where
        c : cs' = cs
        ic'     = ic + 1

tokenizeEscapeChar :: (char -> tokenizer) -> tokenizer
tokenizeEscapeChar k il ic [] = tokError "expected '" il ic
tokenizeEscapeChar k il ic cs
    = esc '\a',                         if c ==. 'a'
    = esc '\b',                         if c ==. 'b'
    = esc '\f',                         if c ==. 'f'
    = esc '\n',                         if c ==. 'n'
    = esc '\r',                         if c ==. 'r'
    = esc '\t',                         if c ==. 't'
    = esc '\v',                         if c ==. 'v'
    = esc '\\',                         if c ==. '\\'
    = tokenizeRadix 16 escn il ic1 cs1, if c ==. 'x'
    = tokenizeRadix 10 escn il ic  cs,  if digit c
    = esc c,                            otherwise
      where
        c : cs1 = cs
        ic1     = ic + 1
        esc c   = k c il ic1 cs1
        escn n il' ic' cs'
             = k (decode n) il' ic' cs',                   if 0 <= n <= 255
             = tokError "illegal character literal" il ic, otherwise

tokenizeLitString :: tokenizer
tokenizeLitString il ic cs
    = tokenizeString checkTerm il ic' cs'
      where
        cs' = tl cs
        ic' = ic + 1

        checkTerm s il1 ic1 cs1
            = tokError "expected \"" il1 ic1,                   if null cs1 \/ c1 ~=. '\"'
            = (TlitString s, il, ic) : tokenizePos il1 ic2 cs2, otherwise
              where
                c1 : cs2   = cs1
                ic2        = ic1 + 1

tokenizeString :: (string -> tokenizer) -> tokenizer
tokenizeString k
    = go []
      where
        go s il ic cs = k (reverse s) il ic cs,      if null cs \/ c ==. '\"' \/ c ==. '\n'
                   = tokenizeChar nextChar il ic cs, otherwise
                     where
                       c          = hd cs
                       nextChar c = go (c : s)
