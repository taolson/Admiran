|| maybeState.m -- functor / applicative / monad / alternative for a state monad augmented with maybe


%export + mst_runState mst_evalState mst_execState

%import <maybe>
%import <state>                 mst_runState/st_runState mst_evalState/st_evalState mst_execState/st_execState

maybeState * ** == state * (maybe **)

|| state access
mst_get :: maybeState * *
mst_get = mb_pure $st_fmap st_get

mst_put :: * -> maybeState * ()
mst_put s' = mb_pure $st_fmap st_put s'

mst_modify :: (* -> *) -> maybeState * ()
mst_modify f = mb_pure $st_fmap st_modify f

mst_pure :: ** -> maybeState * **
mst_pure a = st_pure . mb_pure $ a

mst_fail :: maybeState * **
mst_fail = st_pure Nothing

mst_maybe :: maybe ** -> maybeState * **
mst_maybe m = st_pure m

mst_lift :: state * ** -> maybeState * **
mst_lift m = mb_pure $st_fmap m


|| monad interface
mst_bind :: maybeState * ** -> (** -> maybeState * ***) -> maybeState * ***
mst_bind m f = m $st_bind fromMaybef mst_fail f

mst_join :: maybeState * (maybeState * **) -> maybeState * **
mst_join m = m $st_bind fromMaybef mst_fail mst_runState

mst_kbind :: (** -> maybeState * ***) -> (*** -> maybeState * ****) -> ** -> maybeState * ****
mst_kbind f g = go where go x = f x $mst_bind g

mst_mapM :: (** -> maybeState * ***) -> [**] -> maybeState * [***]
mst_mapM f = foldr (mst_liftA2 (:) . f) (mst_pure [])

mst_mapM_ :: (** -> maybeState * ***) -> [**] -> maybeState * ()
mst_mapM_ f = foldr (mst_right . f) (mst_pure ())

mst_filterM :: (** -> maybeState * bool) -> [**] -> maybeState * [**]
mst_filterM p
    = foldr check (mst_pure [])
      where
        check x       = mst_liftA2 (cond x) (p x)
        cond  x False = id
        cond  x True  = (x :)

mst_foldM :: (*** -> ** -> maybeState * ***) -> *** -> [**] -> maybeState * ***
mst_foldM f z0 xs
    = foldr c mst_pure xs z0
      where
        c x k z = f z x $mst_bind k

mst_sequence :: [maybeState * **] -> maybeState * [**]
mst_sequence xs = foldr (mst_liftA2 (:)) (mst_pure []) xs


|| functor interface
mst_fmap :: (** -> ***) -> maybeState * ** -> maybeState * ***
mst_fmap f m = m $mst_bind (mst_pure . f)


|| applicative interface
mst_apply :: maybeState * (** -> ***) -> maybeState * ** -> maybeState * ***
mst_apply mf ma = mf $mst_bind ($mst_fmap ma)

mst_liftA2 :: (** -> *** -> ****) -> maybeState * ** -> maybeState * *** -> maybeState * ****
mst_liftA2 f ma mb = mst_fmap f ma $mst_apply mb

mst_liftA3 :: (** -> *** -> **** -> *****) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * *****
mst_liftA3 f ma mb mc = mst_liftA2 f ma mb $mst_apply mc

mst_liftA4 :: (** -> *** -> **** -> ***** -> ******) -> maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> maybeState * ******
mst_liftA4 f ma mb mc md = mst_liftA3 f ma mb mc $mst_apply md

mst_left :: maybeState * ** -> maybeState * *** -> maybeState * **
mst_left ma mb = mst_liftA2 const ma mb

mst_right :: maybeState * ** -> maybeState * *** -> maybeState * ***
mst_right ma mb = mst_liftA2 (converse const) ma mb


|| lift multiple independent state actions and bind them to an N-ary function
mst_bind2 :: maybeState * ** -> maybeState * *** -> (** -> *** -> maybeState * ****) -> maybeState * ****
mst_bind2 ma mb f = mst_join (mst_liftA2 f ma mb)

mst_bind3 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> (** -> *** -> **** -> maybeState * *****) -> maybeState * *****
mst_bind3 ma mb mc f = mst_join (mst_liftA3 f ma mb mc)

mst_bind4 :: maybeState * ** -> maybeState * *** -> maybeState * **** -> maybeState * ***** -> (** -> *** -> **** -> ***** ->
             maybeState * ******) -> maybeState * ******
mst_bind4 ma mb mc md f = mst_join (mst_liftA4 f ma mb mc md)


|| alternative interface
mst_alt :: maybeState * ** -> maybeState * ** -> maybeState * **
mst_alt ma mb
    = go
      where
        go s = alt $ mst_runState ma s
               where
                 alt (Nothing, _) = mst_runState mb s
                 alt ra           = ra

mst_some :: maybeState * ** -> maybeState * [**]
mst_some ma = mst_liftA2 (:) ma (mst_many ma)

mst_many :: maybeState * ** -> maybeState * [**]
mst_many ma = mst_some ma $mst_alt mst_pure []
