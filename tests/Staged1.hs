module Staged1(main) where
import Staged

-- The "hello world" of staging: exponentiation with a static exponent.
-- power is a meta level function: it runs at compile time and generates code.
power :: Int -> Code Int -> Code Int
power 0 _ = [| 1 |]
power n x = [| ~x * ~(power (n - 1) x) |]

-- An object level function; the splice is executed by the compiler.
cube :: Int -> Int
cube x = ~(power 3 [| x |])

-- Meta level let, object level body.
power5 :: Int -> Int
power5 x = let n = 2 + 3 in ~(power n [| x |])

-- Compile time constant folding: the sum is computed by the compiler.
staticSum :: Int
staticSum = ~(codeInt (sum [1 .. 100]))

-- Stage polymorphic functions (map, foldr, ...) are available at both stages.
-- Here we unroll a loop over a static list.
sumSquares :: [Int] -> Code Int -> Code Int
sumSquares [] _ = [| 0 |]
sumSquares (k : ks) x = [| ~(codeInt k) * ~x * ~x + ~(sumSquares ks x) |]

poly :: Int -> Int
poly x = ~(sumSquares [1, 2, 3] [| x |])

-- Meta level case with object level branches.
choose :: Bool -> Int -> Int
choose b x = case b of
               True  -> x + 1
               False -> x - 1

useChoose :: Int -> Int
useChoose x = ~(if even (3 :: Int) then [| choose True x |] else [| choose False x |])

main :: IO ()
main = do
  print (cube 3)
  print (power5 2)
  print staticSum
  print (poly 2)
  print (useChoose 10)
