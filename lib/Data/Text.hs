module Data.Text(
  Text,
  StrictText,
  pack, unpack,
  show,
  empty,
  singleton,
  append,
  null,
  length,
  head,
  tail,
  cons,
  snoc,
  uncons,
  replicate,
  splitOn,
  dropWhileEnd,
  words,
  foldr,
  concat,
  lines,
  unlines,
  take,
  drop,
  takeWhile,
  dropWhile,
  dropWhileEnd,
  intercalate,
  isPrefixOf,
  isSuffixOf,
  isInfixOf,
  replace,
  map,
  concatMap,
  foldl,
  foldl',
  unwords,
  toLower,
  toUpper,
  strip,
  stripStart,
  stripEnd,
  dropAround,
  stripPrefix,
  stripSuffix,
  any,
  all,
  filter,
  reverse,
  last,
  init,
  elem,
  zip,
  span,
  break,
  breakOn,
  takeWhileEnd,
  count,
  index,
  chunksOf,
  ) where
import qualified Prelude(); import MiniPrelude hiding(head, tail, null, length, words, map,
  concatMap, foldl, unwords, any, all, filter, reverse, last, init, elem, zip, span, break)
import qualified Data.Char as C
import Control.DeepSeq.Class
import qualified Data.List as L
import Data.String
import qualified Data.ByteString.Internal as BS
import Data.Text.Internal

type StrictText = Text

instance Eq Text where
  (==) = cmp (==)
  (/=) = cmp (/=)

instance Ord Text where
  (<)  = cmp (<)
  (<=) = cmp (<=)
  (>)  = cmp (>)
  (>=) = cmp (>=)

show :: Show a => a -> Text
show = pack . MiniPrelude.show

cmp :: (BS.ByteString -> BS.ByteString -> Bool) -> (Text -> Text -> Bool)
cmp op (T x) (T y) = op x y

instance Show Text where
  showsPrec p = showsPrec p . unpack

instance IsString Text where
  fromString = pack

instance Semigroup Text where
  (<>) = append

instance Monoid Text where
  mempty = empty

instance NFData Text where
  rnf (T bs) = seq bs ()

empty :: Text
empty = pack []

singleton :: Char -> Text
singleton c = pack [c]

pack :: String -> Text
pack s = T (_primitive "toUTF8" s)

unpack :: Text -> String
unpack (T t) = _primitive "fromUTF8" t

append :: Text -> Text -> Text
append (T x) (T y) = T (BS.append x y)

null :: Text -> Bool
null (T bs) = BS.null bs

length :: Text -> Int
length = L.length . unpack

head :: Text -> Char
head (T t)
  | BS.null t = error "Data.Text.head: empty"
  | otherwise = _primitive "headUTF8" t

cons :: Char -> Text -> Text
cons c t = singleton c `append` t

snoc :: Text -> Char -> Text
snoc t c = t `append` singleton c

tail :: Text -> Text
tail (T t)
  | BS.null t = error "Data.Text.tail: empty"
  | otherwise = _primitive "tailUTF8" t

uncons :: Text -> Maybe (Char, Text)
uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)

replicate :: Int -> Text -> Text
replicate = stimes

splitOn :: Text -> Text -> [Text]
splitOn s t = L.map pack $ splitOnList (unpack s) (unpack t)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = pack . L.dropWhileEnd p . unpack

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList [] = error "splitOn: empty"
splitOnList sep = loop []
  where
    loop r  [] = [L.reverse r]
    loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = L.reverse r : loop [] t
                     | otherwise = loop (c:r) cs

words :: Text -> [Text]
words = L.map pack . L.words . unpack

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z = L.foldr f z . unpack

concat :: [Text] -> Text
concat = L.foldr append empty

unlines :: [Text] -> Text
unlines = L.foldr (\ l -> append (append l (pack "\n"))) empty

lines :: Text -> [Text]
lines = L.map pack . L.lines . unpack

take :: Int -> Text -> Text
take n = pack . L.take n . unpack

drop :: Int -> Text -> Text
drop n = pack . L.drop n . unpack

intercalate :: Text -> [Text] -> Text
intercalate _ [] = empty
intercalate _ [x] = x
intercalate s (x:xs) = x `append` s `append` intercalate s xs

replace :: Text -> Text -> Text -> Text
replace s r = intercalate r . splitOn s

-- XXX Should make the BS version efficient and go via that
isPrefixOf :: Text -> Text -> Bool
isPrefixOf p s = L.isPrefixOf (unpack p) (unpack s)

isSuffixOf :: Text -> Text -> Bool
isSuffixOf p s = L.isSuffixOf (unpack p) (unpack s)

isInfixOf :: Text -> Text -> Bool
isInfixOf p s = L.isInfixOf (unpack p) (unpack s)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = pack . L.dropWhile p . unpack

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = pack . L.takeWhile p . unpack

map :: (Char -> Char) -> Text -> Text
map f = pack . L.map f . unpack

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . L.map f . unpack

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = L.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

unwords :: [Text] -> Text
unwords = intercalate (pack " ")

toLower :: Text -> Text
toLower = map C.toLower

toUpper :: Text -> Text
toUpper = map C.toUpper

stripStart :: Text -> Text
stripStart = dropWhile C.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd C.isSpace

strip :: Text -> Text
strip = stripEnd . stripStart

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhileEnd p . dropWhile p

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t = fmap pack (L.stripPrefix (unpack p) (unpack t))

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t = fmap (pack . L.reverse) (L.stripPrefix (L.reverse (unpack p)) (L.reverse (unpack t)))

any :: (Char -> Bool) -> Text -> Bool
any p = L.any p . unpack

all :: (Char -> Bool) -> Text -> Bool
all p = L.all p . unpack

filter :: (Char -> Bool) -> Text -> Text
filter p = pack . L.filter p . unpack

reverse :: Text -> Text
reverse = pack . L.reverse . unpack

last :: Text -> Char
last = L.last . unpack

init :: Text -> Text
init = pack . L.init . unpack

elem :: Char -> Text -> Bool
elem c = L.elem c . unpack

zip :: Text -> Text -> [(Char, Char)]
zip a b = L.zip (unpack a) (unpack b)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = case L.span p (unpack t) of (a, b) -> (pack a, pack b)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)

-- | Split at the first occurrence of the pattern (which is part of the second component).
breakOn :: Text -> Text -> (Text, Text)
breakOn p t = go [] (unpack t)
  where ps = unpack p
        go acc s@(c:cs) | ps `L.isPrefixOf` s = (pack (L.reverse acc), pack s)
                        | otherwise = go (c:acc) cs
        go acc [] = (pack (L.reverse acc), empty)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = pack . L.reverse . L.takeWhile p . L.reverse . unpack

-- | Number of non-overlapping occurrences of the pattern.
count :: Text -> Text -> Int
count p t = L.length (splitOn p t) - 1

index :: Text -> Int -> Char
index t i = unpack t L.!! i

chunksOf :: Int -> Text -> [Text]
chunksOf n t | null t = []
             | otherwise = take n t : chunksOf n (drop n t)
