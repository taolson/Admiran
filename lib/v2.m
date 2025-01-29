||  v2.m -- 2D vectors and associated operations


%export +

%import <lens>
%import <maybe>
%import <mirandaExtensions>


v2 * ::= V2 * *

|| lenses for v2
viewV2_0 (V2 a b) = a; overV2_0 fn (V2 a b) = V2 (fn a) b; lensV2_0 = Lens viewV2_0 overV2_0
viewV2_1 (V2 a b) = b; overV2_1 fn (V2 a b) = V2 a (fn b); lensV2_1 = Lens viewV2_1 overV2_1


|| monad interface

v2_bind :: v2 * -> (* -> v2 **) -> v2 **
v2_bind (V2 a b) f
    = V2 a' b'
      where
        V2 a' _ = f a
        V2 _ b' = f b

v2_pure :: * -> v2 *
v2_pure x = V2 x x

v2_sequence :: [v2 *] -> v2 [*]
v2_sequence xs = foldr (v2_liftA2 (:)) (v2_pure []) xs


|| functor interface
v2_fmap :: (* -> **) -> v2 * -> v2 **
v2_fmap f (V2 a b) = V2 (f a) (f b)


|| applicative interface
v2_apply :: v2 (* -> **) -> v2 * -> v2 **
v2_apply (V2 fa fb) (V2 a b) = V2 (fa a) (fb b)

v2_liftA2 :: (* -> ** -> ***) -> v2 * -> v2 ** -> v2 ***
v2_liftA2 f (V2 a b) (V2 c d) = V2 (f a c) (f b d)


|| foldable interface
v2_foldr :: (* -> ** -> **) -> ** -> v2 * -> **
v2_foldr f z (V2 a b) = f a (f b z)

v2_foldl :: (** -> * -> **) -> ** -> v2 * -> **
v2_foldl f z (V2 a b) = f (f z a) b


v2_abs, v2_neg, v2_signum :: v2 int -> v2 int
v2_abs    a  = v2_fmap abs a
v2_neg    a  = v2_fmap neg a
v2_signum a  = v2_fmap signum a

v2_add, v2_sub, v2_mul, v2_div :: v2 int -> v2 int -> v2 int
v2_add a b = v2_liftA2 (+) a b
v2_sub a b = v2_liftA2 (-) a b
v2_mul a b = v2_liftA2 (*) a b
v2_div a b = v2_liftA2 div a b
v2_mod a b = v2_liftA2 mod a b

v2_max, v2_min :: ordI * -> v2 * -> v2 * -> v2 *
v2_max cmp a b = v2_liftA2 (max2 cmp) a b
v2_min cmp a b = v2_liftA2 (min2 cmp) a b

v2_sum, v2_product :: v2 int -> int
v2_sum     a = v2_foldl (+) 0 a
v2_product a = v2_foldl (*) 1 a

|| manhattan distance
v2_dist :: v2 int -> v2 int -> int
v2_dist a b = v2_sub a b |> v2_abs |> v2_sum
