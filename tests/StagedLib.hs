-- Meta level definitions used from another module (tests/Staged3.hs).
module StagedLib(power, unrollSum, Vec(..), vmap, vsum) where
import Staged

power :: Int -> Code Int -> Code Int
power 0 _ = [| 1 |]
power n x = [| ~x * ~(power (n - 1) x) |]

-- Unroll a sum over a static range using Prelude functions at compile time.
unrollSum :: Int -> Code (Int -> Int) -> Code Int
unrollSum n f = foldr (\ i acc -> [| ~f ~(codeInt i) + ~acc |]) [| 0 |] [1 .. n]

-- Statically sized vectors: a meta level data type holding object level code.
data Vec a = VNil | VCons (Code a) (Vec a)

vmap :: (Code a -> Code b) -> Vec a -> Vec b
vmap _ VNil = VNil
vmap f (VCons x xs) = VCons (f x) (vmap f xs)

vsum :: Vec Int -> Code Int
vsum VNil = [| 0 |]
vsum (VCons x xs) = [| ~x + ~(vsum xs) |]
