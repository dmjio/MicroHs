-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
-- Functions for GHC that are defined in the UHS libs.
module Compat(module Compat) where
--import Control.Exception
import Data.Char
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
--import qualified Control.Monad as M
import Control.Exception
import Data.List
import System.Environment
import System.IO

------- Int --------

_integerToInt :: Integer -> Int
_integerToInt = fromInteger

_integerToDouble :: Integer -> Double
_integerToDouble = fromIntegral

-- Same as in Data.Integer
_integerToIntList :: Integer -> [Int]
_integerToIntList i | i < 0 = -1 : to (-i)
                    | otherwise =  to i
  where to 0 = []
        to n = fromInteger r : to q  where (q, r) = quotRem n 2147483648

------- List --------

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

-- A simple "quicksort" for now.
sortLE :: forall a . (a -> a -> Bool) -> [a] -> [a]
sortLE _  [] = []
sortLE le (x:xs) = sortLE le lt ++ (x : sortLE le ge)
  where (ge, lt) = partition (le x) xs

showListS :: (a -> String) -> [a] -> String
showListS sa arg =
  let
    showRest as =
      case as of
        [] -> "]"
        x : xs -> "," ++ sa x ++ showRest xs
  in
    case arg of
      [] -> "[]"
      a : as -> "[" ++ sa a ++ showRest as

anySame :: (Eq a) => [a] -> Bool
anySame = anySameBy (==)

anySameBy :: (a -> a -> Bool) -> [a] -> Bool
anySameBy _ [] = False
anySameBy eq (x:xs) = elemBy eq x xs || anySameBy eq xs

deleteAllBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteAllBy _ _ [] = []
deleteAllBy eq x (y:ys) = if eq x y then deleteAllBy eq x ys else y : deleteAllBy eq x ys

deleteAllsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteAllsBy eq = foldl (flip (deleteAllBy eq))

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

------- Exception --------

newtype Exn = Exn String
  deriving (Show)
instance Exception Exn

------- IO --------

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m) :: IO (Either IOError Handle)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

getTimeMilli :: IO Int
getTimeMilli  = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

-- A hack until we have a real withArgs
withDropArgs :: Int -> IO a -> IO a
withDropArgs i ioa = do
  as <- getArgs
  withArgs (drop i as) ioa

------- Read --------

readInteger :: String -> Integer
readInteger = read

-- Convert string in scientific notation to a rational number.
readRational :: String -> Rational
readRational "" = undefined
readRational acs@(sgn:as) | sgn == '-' = negate $ rat1 as
                          | otherwise  =          rat1 acs
  where
    rat1 s1 =
      case span isDigit s1 of
        (ds1, cr1) | ('.':r1) <- cr1                   -> rat2 f1 r1
                   | (c:r1)   <- cr1, toLower c == 'e' -> rat3 f1 r1
                   | otherwise                         -> f1
          where f1 = toRational (readInteger ds1)

    rat2 f1 s2 =
      case span isDigit s2 of
        (ds2, cr2) | (c:r2) <- cr2, toLower c == 'e' -> rat3 f2 r2
                   | otherwise                       -> f2
          where f2 = f1 + toRational (readInteger ds2) * 10 ^^ (negate $ length ds2)

    rat3 f2 ('+':s) = f2 * expo s
    rat3 f2 ('-':s) = f2 / expo s
    rat3 f2      s  = f2 * expo s

    expo s = 10 ^ readInteger s

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  b <- p x
  (ts,fs) <- partitionM p xs
  return $ if b then (x:ts, fs) else (ts, x:fs)

substString :: forall a . Eq a => [a] -> [a] -> [a] -> [a]
substString _ _ [] = []
substString from to xs@(c:cs) | Just rs <- stripPrefix from xs = to ++ substString from to rs
                              | otherwise = c : substString from to cs

openTmpFile :: String -> IO (String, Handle)
openTmpFile tmplt = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "" tmplt

hSerialize :: Handle -> a -> IO ()
hSerialize _ _ = error "ghc: hSerialize"

hDeserialize :: Handle -> IO a
hDeserialize _ = error "ghc: hDeserialize"

usingMhs :: Bool
usingMhs = False

_wordSize :: Int
_wordSize = 64

_isWindows :: Bool
_isWindows = False
