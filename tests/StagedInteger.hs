module StagedInteger(main) where
import Data.Bits
import Data.Int
import Data.Word
import Math.NumberTheory.Logarithms(integerLog2)
import Staged

-- Meta level code is run by the runtime system, so Integer at compile time is
-- the same implementation (imath, GMP, or Haskell) as at run time.
-- results is stage polymorphic: it is computed both by the compiler (ctResults)
-- and by the program (rtResults), and the two must agree.

values :: [Integer]
values = [0, 1, -1, 7, -7, 2^31, -(2^31), 2^63 - 1, -(2^63), 2^64 + 3, -(2^64 + 3), 3^50, -(3^50)]

divisors :: [Integer]
divisors = [1, -1, 3, -3, 2^32 + 1, -(2^32 + 1), 5^30]

shifts :: [Int]
shifts = [0, 1, 5, 63, 64, 70]

results :: [String]
results =
  [ unwords [show x, show y, show (x + y), show (x - y), show (x * y), show (compare x y), show (x == y)]
  | x <- values, y <- values ] ++
  [ unwords [show x, show y, show (quotRem x y), show (divMod x y)]
  | x <- values, y <- divisors ] ++
  [ unwords [show x, show y, show (x .&. y), show (x .|. y), show (xor x y)]
  | x <- values, y <- values ] ++
  [ unwords [show x, show k, show (shiftL x k), show (shiftR x k), show (testBit x k)]
  | x <- values, k <- shifts ] ++
  [ unwords [show x, show (negate x), show (abs x), show (signum x), show (popCount x)]
  | x <- values ] ++
  [ unwords [show x, show (integerLog2 x)]
  | x <- values, x > 0 ] ++
  [ unwords [show x, show (fromInteger x :: Int), show (fromInteger x :: Word),
             show (fromInteger x :: Int64), show (fromInteger x :: Word64),
             show (fromInteger x :: Double), show (fromInteger x :: Float)]
  | x <- values ] ++
  [ unwords [show (toInteger (minBound :: Int)), show (toInteger (maxBound :: Int)),
             show (toInteger (maxBound :: Word)), show (toInteger (minBound :: Int64)),
             show (toInteger (maxBound :: Word64))] ] ++
  [ show (product [1 .. 30 :: Integer]), show (2 ^ (200 :: Int) :: Integer), show (read "-123456789012345678901234567890" :: Integer) ]

ctResults :: String
ctResults = ~(codeString (unlines results))

-- Compile time Integers as object level Integers.
ctValues :: [Integer]
ctValues = ~(codeList (map codeInteger values))

big :: Integer
big = ~(codeInteger (2 ^ (100 :: Int) + 7))

negBig :: Integer
negBig = ~(codeInteger (negate (3 ^ (60 :: Int))))

small :: Integer
small = ~(codeInteger (sum [1 .. 100]))

main :: IO ()
main = do
  let ct = lines ctResults
      rt = results
      bad = [ (c, r) | (c, r) <- zip ct rt, c /= r ]
  print (length ct, length rt)
  mapM_ (\ (c, r) -> putStrLn ("compile time: " ++ c ++ "\nrun time:     " ++ r)) bad
  putStrLn (if null bad && length ct == length rt then "compile time and run time agree" else "MISMATCH")
  print (ctValues == values)
  print big
  print (big == 2 ^ (100 :: Int) + 7)
  print negBig
  print small
