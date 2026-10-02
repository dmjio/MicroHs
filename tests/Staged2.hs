module Staged2(main) where
import Staged

-- Partially static lists: a meta level list of object level elements.
-- The length is known at compile time, the elements are not.
data SList a = SNil | SCons (Code a) (SList a)

sfromList :: [Code a] -> SList a
sfromList = foldr SCons SNil

smap :: (Code a -> Code b) -> SList a -> SList b
smap _ SNil = SNil
smap f (SCons x xs) = SCons (f x) (smap f xs)

ssum :: SList Int -> Code Int
ssum SNil = [| 0 |]
ssum (SCons x xs) = [| ~x + ~(ssum xs) |]

-- Unrolled dot product of two 3-vectors, generated at compile time.
dot3 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
dot3 a b c x y z = ~(ssum (szip (sfromList [ [|a|], [|b|], [|c|] ]) (sfromList [ [|x|], [|y|], [|z|] ])))
  where
    szip (SCons p ps) (SCons q qs) = SCons [| ~p * ~q |] (szip ps qs)
    szip _ _ = SNil

-- Inlined map: the function argument is inlined into the generated loop.
inlMap :: (Code a -> Code b) -> Code [a] -> Code [b]
inlMap f xs = [| let go [] = []
                     go (y : ys) = ~(f [| y |]) : go ys
                 in go ~xs |]

addTen :: [Int] -> [Int]
addTen xs = ~(inlMap (\ x -> [| ~x + 10 |]) [| xs |])

-- Let insertion with the Gen monad: the expensive expression is bound once.
square :: Code Int -> Gen (Code Int)
square c = do
  x <- gen c
  return [| ~x * ~x |]

squareOfSum :: Int -> Int -> Int
squareOfSum a b = ~(runGen (square [| a + b |]))

-- Object level case inside a meta level function: the case ends up in the code.
safeDiv :: Code Int -> Code Int -> Code Int
safeDiv x y = [| case ~y of
                   0 -> 0
                   d -> ~x `div` d |]

useSafeDiv :: Int -> Int -> Int
useSafeDiv a b = ~(safeDiv [| a |] [| b |])

-- Compile time computation with the Prelude: a lookup table generated at compile time.
fibs :: [Int]
fibs = ~(codeList (take 10 (go 0 1)))
  where go a b = a : go b (a + b)
        codeList [] = [| [] |]
        codeList (n : ns) = [| ~(codeInt n) : ~(codeList ns) |]

main :: IO ()
main = do
  print (dot3 1 2 3 4 5 6)
  print (addTen [1, 2, 3])
  print (squareOfSum 3 4)
  print (useSafeDiv 10 2, useSafeDiv 10 0)
  print fibs
