module Staged4(main) where
import Data.Char
import Data.Int
import Staged

-- Meta level code is run by the runtime system, so everything the runtime
-- can do is available at compile time.

ints :: [Int]
ints = [1, 2, 3]

-- An overloaded function used directly in a splice.
total :: Int
total = ~(codeInt (sum (map fromIntegral ints)))

-- Integer uses the FFI (imath or GMP).
big :: String
big = ~(codeString (show (product [1 .. 25 :: Integer])))

-- So does show for Double.
root :: String
root = ~(codeString (show (sqrt 2 :: Double)))

bounds :: String
bounds = ~(codeString (show (minBound :: Int32, maxBound :: Int32)))

-- The character tables are byte strings.
shout :: String
shout = ~(codeString (map toUpper "hello"))

-- Code for a list, and for an Integer.
squares :: [Int]
squares = ~(codeList [ codeInt (i * i) | i <- [1 .. 5] ])

huge :: Integer
huge = ~(codeInteger (2 ^ (100 :: Int) + 7))

-- A splice with several object level variables.
dot3 :: Int -> Int -> Int -> Int
dot3 x y z = ~(foldr (\ (k, c) r -> [| ~(codeInt k) * ~c + ~r |]) [| 0 |] (zip [1 ..] [[| x |], [| y |], [| z |]]))

main :: IO ()
main = do
  print total
  putStrLn big
  putStrLn root
  putStrLn bounds
  putStrLn shout
  print squares
  print huge
  print (dot3 1 10 100)
