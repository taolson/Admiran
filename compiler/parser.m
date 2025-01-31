|| parser.m -- basic parser combinators for a stream of tokens with locations


%export + p_get p_put p_modify p_bind p_pure p_fail p_fmap p_apply p_liftA2 p_liftA3 p_liftA4 p_left p_right p_bind2 p_mapM p_foldM p_runState

%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <maybeState>            p_get/mst_get p_put/mst_put p_modify/mst_modify p_bind/mst_bind p_pure/mst_pure p_fail/mst_fail p_fmap/mst_fmap
                                p_apply/mst_apply p_liftA2/mst_liftA2 p_liftA3/mst_liftA3 p_liftA4/mst_liftA4 p_left/mst_left p_right/mst_right
                                p_bind2/mst_bind2 p_mapM/mst_mapM p_foldM/mst_foldM p_runState/mst_runState
%import "name"
%import "tokenizer"


|| Basic Parsing

psError == (string, int, int, int)              || err string, severity, line#, col#

|| custom comparison of psError that only looks at line, col, and severity
|| used to choose the parse state with the deepest error in a p_alt
cmppsError :: ordI psError
cmppsError (_, s1, l1, c1) (_, s2, l2, c2)
    = cmpint l1 l2 $thenCmp cmpint c1 c2 $thenCmp cmpint s1 s2

emptyError :: psError
emptyError  =  ("", 0, 0, 0)

|| state passed through the various parsers
psSt    == (string, psError, [int], [tokloc])   || module name, deepest parse error, indent-level stack, stream of tokens and locations

|| lenses for psSt fields
psModName = lensTup4_0
psErr     = lensTup4_1
psErrStr  = composeLens lensTup4_1 lensTup4_0
psErrSev  = composeLens lensTup4_1 lensTup4_1
psErrLn   = composeLens lensTup4_1 lensTup4_2
psErrCn   = composeLens lensTup4_1 lensTup4_3
psIndLev  = lensTup4_2
psToks    = lensTup4_3

parser * == maybeState psSt *


|| parser error handling

|| report an error with a specified severity level and error string,
|| keeping the deepest error of that and the existing psErr
p_error :: int -> string -> parser *
p_error sv es ps
    = (Nothing, ps')
      where
        (_, il, ic) = hd $ view psToks ps
        ps'         = over psErr (max2 cmppsError (es, sv, il, ic)) ps

|| show the current token, or a string indicating an exception condition
|| (end of input, offside)
p_showCurrentToken :: parser string
p_showCurrentToken
    = p_get $p_bind go
      where
        go ps
            = p_pure "end of input", if isTend t
            = p_pure "OFFSIDE",      if _eq cmptoken t Toffside \/ offside
            = p_pure $ showToken t,  otherwise
              where
                ls          = view psIndLev ps
                (t, _, ic)  = hd $ view psToks ps
                offside     = ~null ls & (isTend t \/ ic < hd ls)

|| report an error that we expected something that wasn't there
p_expected :: string -> parser *
p_expected s  = p_showCurrentToken $p_bind (p_error 2 . ("expected " ++ s ++ ", but got " ++))

|| report an error that we got an unexpected token
p_unexpected :: parser *
p_unexpected = p_showCurrentToken $p_bind (p_error 1 . ("unexpected " ++))

|| alternative interface
|| note: a simple alias to maybeState mst_alt can't be used, here, because we need to run the
|| alternative leg with the error state of the primary leg to keep the deepest error
p_alt :: parser * -> parser * -> parser *
p_alt pa pb
    = go
      where
        go ps
            = alt $ p_runState pa ps
              where
                alt (Nothing, psa) = p_runState pb (set psErr (view psErr psa) ps)
                alt ra             = ra

|| p_peek and p_not check for the presence or absence of a token, but don't consume it
p_peek, p_not :: parser * -> parser ()
p_peek pa ps
    = (Just (), ps), if isJust ra
    = (Nothing, ps), otherwise
      where
        ra = fst $ pa ps

p_not pa ps
    = (Just (), ps), if isNothing ra
    = (Nothing, ps), otherwise
      where
        ra = fst $ pa ps

p_optional :: parser * -> parser (maybe *)
p_optional pa = p_fmap Just pa $p_alt p_pure Nothing

|| run a parser and check the result against a guard predicate.  If the guard
|| fails, rewind the parser to the original point (using p_alt) and report
|| unexpected input
p_guard :: parser * -> (* -> parser **) -> parser **
p_guard p g
    = (p $p_bind g) $p_alt p_unexpected

|| parser for a token which satifies a condition
p_satisfy :: parser * -> (* -> bool) -> parser *
p_satisfy p tst
    = p $p_guard f
      where
        f x = p_pure x, if tst x
            = p_fail,   otherwise

|| 1 or more p
p_some :: parser * -> parser [*]
p_some p = p_liftA2 (:) p (p_many p)

|| 0 or more p
p_many :: parser * -> parser [*]
p_many p = p_some p $p_alt p_pure []

|| 1 or more p seperated by ps
p_someSepBy :: parser * -> parser ** -> parser [**]
p_someSepBy ps p = p_liftA2 (:) p (p_many (ps $p_right p))

|| 0 or more p seperated by ps
p_manySepBy :: parser * -> parser ** -> parser [**]
p_manySepBy ps p = p_someSepBy ps p $p_alt p_pure []


|| token parsers

|| parser which accepts any token
|| also handle offside detection by inserting a Toffside token into the token stream
|| Note: since p_any always accepts a token, any subsequent guard or check for validity
|| should use "p_fail" for the failure, and use a p_alt to rewind the token stream to the
|| original position to correctly report the error location and token
p_any :: parser token
p_any ps
    = p_unexpected ps,    if isTend t & null ls || end of token stream
    = p_error 2 err ps,   if isTerror t         || error from lexer
    = (Just t', ps'),     otherwise
      where
        ls                = view psIndLev ps
        (t, _, ic) : ts'  = view psToks ps
        Terror err        = t
        offside           = ~null ls & (isTend t \/ ic < hd ls)
        t'                = Toffside,          if offside
                          = t,                 otherwise
        ps'               = ps,                if offside
                          = set psToks ts' ps, otherwise

|| parser which passes only at end of input
p_end :: parser ()
p_end = p_not p_any

|| parser which matches a specific token or posts an error
p_token :: token -> parser token
p_token t = p_any $p_satisfy (_eq cmptoken t)

|| parser which matches a specific character
p_char :: char -> parser token
p_char c = p_token (Tchar c)

|| parser which matches a specific identifier string
p_ident :: string -> parser token
p_ident s = p_token (Tident (Unqualified s))

|| parser which matches a specific symbol
p_sym :: string -> parser token
p_sym s = p_token (Tsymbol (Unqualified s))

p_semi, p_comma, p_equal, p_eqeq, p_minus, p_percent, p_colons, p_lArrow, p_rArrow, p_vBar, p_terminator :: parser token
p_semi       = p_char ';'
p_comma      = p_char ','
p_equal      = p_sym "="
p_eqeq       = p_sym "=="
p_minus      = p_sym "-"
p_percent    = p_sym "%"
p_colons     = p_sym "::"
p_lArrow     = p_sym "<-"
p_rArrow     = p_sym "->"
p_vBar       = p_sym "|"
p_terminator = p_token Toffside $p_alt p_semi


|| structure parsers

|| ( parser )
p_inParens :: parser * -> parser *
p_inParens p = p_char '(' $p_right p $p_left p_char ')'

|| [ parser ]
p_inBrackets :: parser * -> parser *
p_inBrackets p = p_char '[' $p_right p $p_left p_char ']'

|| { parser }
p_inBraces :: parser * -> parser *
p_inBraces p = p_char '{' $p_right p $p_left p_char '}'

|| < parser >
p_inAngles :: parser * -> parser *
p_inAngles p = p_sym "<" $p_right p $p_left p_sym ">"

|| block layout parser
p_inLayout :: parser * -> parser *
p_inLayout p = p_indent $p_right p $p_left p_outdent

|| push the current token column on the indent level stack in the parser state
p_indent :: parser ()
p_indent ps
    = (Just (), over psIndLev (ic :) ps)
      where
        (_, _, ic) = hd $ view psToks ps        || get current token indent column ic

|| check for separator and pop the indent level stack in the parser state
p_outdent :: parser ()
p_outdent
    = (p_terminator $p_right p_get) $p_bind (p_put . over psIndLev popLev)
      where
        popLev []       = []
        popLev (x : xs) = xs
