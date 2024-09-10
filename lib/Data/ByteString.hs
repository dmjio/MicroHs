module Data.ByteString(
  ByteString,
  pack, unpack,
  empty,
  append, append3,
  length,
  substr,
  ) where
import Prelude hiding ((++), length)
import Data.Monoid
import Data.Semigroup
import Data.String
import Data.Word(Word8)

data ByteString  -- primitive type

primBSappend  :: ByteString -> ByteString -> ByteString
primBSappend  = primitive "bs++"
primBSappend3 :: ByteString -> ByteString -> ByteString -> ByteString
primBSappend3 = primitive "bs+++"
primBSEQ      :: ByteString -> ByteString -> Bool
primBSEQ      = primitive "bs=="
primBSNE      :: ByteString -> ByteString -> Bool
primBSNE      = primitive "bs/="
primBSLT      :: ByteString -> ByteString -> Bool
primBSLT      = primitive "bs<"
primBSLE      :: ByteString -> ByteString -> Bool
primBSLE      = primitive "bs<="
primBSGT      :: ByteString -> ByteString -> Bool
primBSGT      = primitive "bs>"
primBSGE      :: ByteString -> ByteString -> Bool
primBSGE      = primitive "bs>="
primBScmp     :: ByteString -> ByteString -> Ordering
primBScmp     = primitive "bscmp"
primBSpack    :: [Word8] -> ByteString
primBSpack    = primitive "bspack"
primBSunpack  :: ByteString -> [Word8]
primBSunpack  = primitive "bsunpack"
primBSlength  :: ByteString -> Int
primBSlength  = primitive "bslength"
primBSsubstr  :: ByteString -> Int -> Int -> ByteString
primBSsubstr  = primitive "bssubstr"

instance Eq ByteString where
  (==) = primBSEQ
  (/=) = primBSNE

instance Ord ByteString where
  compare = primBScmp
  (<)     = primBSLT
  (<=)    = primBSLE
  (>)     = primBSGT
  (>=)    = primBSGE

instance Show ByteString where
  showsPrec p bs = showsPrec p (map (toEnum . fromEnum) (unpack bs) :: [Char])

instance IsString ByteString where
  fromString = pack . map (toEnum . fromEnum)

instance Semigroup ByteString where
  (<>) = append

instance Monoid ByteString where
  mempty = empty

empty :: ByteString
empty = pack []

append :: ByteString -> ByteString -> ByteString
append = primBSappend

append3 :: ByteString -> ByteString -> ByteString -> ByteString
append3 = primBSappend3

pack :: [Word8] -> ByteString
pack = primBSpack

unpack :: ByteString -> [Word8]
unpack = primBSunpack

length :: ByteString -> Int
length = primBSlength

substr :: ByteString -> Int -> Int -> ByteString
substr bs offs len
  | offs < 0 || offs > sz     = error "Data.ByteString.substr bad offset"
  | len < 0  || len > sz-offs = error "Data.ByteString.substr bad length"
  | otherwise = primBSsubstr bs offs len
  where sz = length bs
