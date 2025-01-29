|| zipper.m -- implementation of a list with a cursor


%export +

%import <mirandaExtensions>


zipper * ::= Zipper [*] ! [*] !

z_empty :: zipper *
z_empty = Zipper [] []

z_null :: zipper * -> bool
z_null (Zipper [] []) = True
z_null _              = False

z_singleton :: * -> zipper *
z_singleton x = Zipper [] [x]

z_fromList :: [*] -> zipper *
z_fromList xs = Zipper [] xs

z_toList :: zipper * -> [*]
z_toList (Zipper ls rs) = reverse ls ++ rs

z_beginp, z_endp :: zipper * -> bool
z_beginp (Zipper [] _) = True
z_beginp _             = False

z_endp  (Zipper _ []) = True
z_endp  _             = False

z_begin, z_end :: zipper * -> zipper *
z_begin (Zipper ls rs) = Zipper [] (reverse ls ++ rs)
z_end   (Zipper ls rs) = Zipper (reverse rs ++ ls) []

z_cursor :: zipper * -> *
z_cursor (Zipper _ (a : _)) = a
z_cursor z                  = error "z_cursor: null zipper"

z_left, z_right :: zipper * -> zipper *
z_left (Zipper (a : ls) rs) = Zipper ls (a : rs)
z_left z                    = z

z_right (Zipper ls (a : rs)) = Zipper (a : ls) rs
z_right z                    = z

|| insert an element before the cursor
z_insert :: * -> zipper * -> zipper *
z_insert a (Zipper ls rs) = Zipper (a : ls) rs

|| delete the element at the cursor
z_delete :: zipper * -> zipper *
z_delete (Zipper ls (_ : rs)) = Zipper ls rs
z_delete z                    = z

|| push an element before the cursor
z_push :: * -> zipper * -> zipper *
z_push a (Zipper ls rs) = Zipper (a : ls) rs

|| delete the element before the cursor
z_pop :: zipper * -> zipper *
z_pop (Zipper (_ : ls) rs) = Zipper ls rs
z_pop z                    = error "z_pop: empty zipper"

|| modify the element at the cursor
z_modify :: (* -> *) -> zipper * -> zipper *
z_modify f (Zipper ls (a : rs)) = Zipper ls (f a : rs)
z_modify f z                    = error "z_modify: empty zipper"

|| functor instance for zipper
z_fmap :: (* -> **) -> zipper * -> zipper **
z_fmap f (Zipper ls rs) = Zipper (map f ls) (map f rs)

|| fold instance for zipper
z_fold :: (** -> * -> **) -> ** -> zipper * -> **
z_fold f z (Zipper ls rs) = foldl f (foldl f z ls) rs
