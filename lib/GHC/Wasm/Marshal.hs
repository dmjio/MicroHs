-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Conversion between Haskell and JavaScript values, and the function behind
-- the [js| ... |] syntax; only available with a JavaScript target
-- (emscripten, browser, ...), like GHC.Wasm.Prim.
--
-- The classes follow GHCJS (GHCJS.Marshal):
--   toJSVal   :: a -> IO JSVal
--   fromJSVal :: JSVal -> IO (Maybe a)      -- Nothing if the value has the wrong type
-- with instances for JSVal, JSString, Text, String, Int, Word, Double, Float,
-- Char, Bool, (), Maybe a (null or undefined is Nothing) and [a] (an array).
--
-- [js| code |] is inline JavaScript: an expression, or statements with 'return',
-- like 'foreign import javascript'.  ${e} in the code is the value of the Haskell
-- expression e, converted with toJSVal; the result is converted with fromJSValUnchecked:
--
--   n <- [js| ${a} + ${b} |] :: IO Int
--   [js| console.log("hello, " + ${name}) |] :: IO ()
--
-- The code is compiled (once per quote) on the JavaScript side when it is first run;
-- it can use 'Module' and 'mhsjs' like a foreign import.  A JavaScript exception in
-- it is raised as a JSException.  Since ${ } is taken by the interpolation, a
-- JavaScript template literal in the code cannot use it.
-- The syntax is only recognized with a target that has a js option in targets.conf,
-- so with other targets [js|x<-xs] is still a list comprehension.
--
-- Mhs.Builtin imports this module with {-# SOURCE #-} (Marshal.hs-boot), and with a
-- JavaScript target every module that imports the Prelude also imports this module
-- (for the instances), so this module and its imports must not import the Prelude.
module GHC.Wasm.Marshal(
  ToJSVal(..),
  FromJSVal(..),
  jsQuote,
  ) where
import qualified Prelude(); import MiniPrelude
import Data.Text(Text)
import Data.Word(Word)
import Data.Double(Double)
import Data.Float(Float)
import GHC.Wasm.Prim
import Primitives(JSVal)

class ToJSVal a where
  toJSVal :: a -> IO JSVal

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  fromJSValUnchecked :: JSVal -> IO a
  fromJSValUnchecked v = do
    m <- fromJSVal v
    case m of
      Just a  -> return a
      Nothing -> error "fromJSValUnchecked: the JavaScript value has the wrong type"

----------------------------------------
-- [js| ... |]

-- | Run the code of a [js| ... |] quote with $1, $2, ... bound to the arguments.
jsQuote :: FromJSVal a => String -> [IO JSVal] -> IO a
jsQuote code args = do
  arr <- js_newArray
  mapM_ (\ a -> a >>= js_push arr) args
  js_inline (toJSString code) arr >>= fromJSValUnchecked

foreign import javascript safe "mhsjs.inline($1, $2)" js_inline :: JSString -> JSVal -> IO JSVal

----------------------------------------
-- Conversions

foreign import javascript "$1" js_fromInt    :: Int    -> IO JSVal
foreign import javascript "$1" js_fromWord   :: Word   -> IO JSVal
foreign import javascript "$1" js_fromDouble :: Double -> IO JSVal
foreign import javascript "$1" js_fromFloat  :: Float  -> IO JSVal
foreign import javascript "$1" js_fromBool   :: Bool   -> IO JSVal
foreign import javascript "String.fromCodePoint($1)" js_fromChar :: Char -> IO JSVal

foreign import javascript "$1" js_toInt    :: JSVal -> IO Int
foreign import javascript "$1" js_toWord   :: JSVal -> IO Word
foreign import javascript "$1" js_toDouble :: JSVal -> IO Double
foreign import javascript "$1" js_toFloat  :: JSVal -> IO Float
foreign import javascript "$1" js_toBool   :: JSVal -> IO Bool
foreign import javascript "$1.codePointAt(0)" js_toChar :: JSVal -> IO Char

foreign import javascript "typeof $1 === 'number'"  js_isNumber  :: JSVal -> IO Bool
foreign import javascript "typeof $1 === 'string'"  js_isString  :: JSVal -> IO Bool
foreign import javascript "typeof $1 === 'boolean'" js_isBoolean :: JSVal -> IO Bool
foreign import javascript "$1 === null || $1 === undefined" js_isNullish :: JSVal -> IO Bool

foreign import javascript "[]"           js_newArray :: IO JSVal
foreign import javascript "$1.push($2)"  js_push     :: JSVal -> JSVal -> IO ()
foreign import javascript "Array.isArray($1)" js_isArray :: JSVal -> IO Bool
foreign import javascript "$1.length"    js_length   :: JSVal -> IO Int
foreign import javascript "$1[$2]"       js_index    :: JSVal -> Int -> IO JSVal

-- Convert if the value passes the test.
checked :: (JSVal -> IO Bool) -> (JSVal -> IO a) -> JSVal -> IO (Maybe a)
checked test conv v = do
  ok <- test v
  if ok then Just <$> conv v else return Nothing

----------------------------------------
-- Instances

instance ToJSVal JSVal where
  toJSVal = return

instance FromJSVal JSVal where
  fromJSVal = return . Just

instance ToJSVal JSString where
  toJSVal (JSString v) = return v

instance FromJSVal JSString where
  fromJSVal = checked js_isString (return . JSString)

instance ToJSVal Text where
  toJSVal = toJSVal . textToJSString

instance FromJSVal Text where
  fromJSVal v = fmap textFromJSString <$> fromJSVal v

-- A String is a JavaScript string, not an array of characters.
instance ToJSVal [Char] where
  toJSVal = toJSVal . toJSString

instance FromJSVal [Char] where
  fromJSVal v = fmap fromJSString <$> fromJSVal v

instance {-# OVERLAPPABLE #-} ToJSVal a => ToJSVal [a] where
  toJSVal xs = do
    arr <- js_newArray
    mapM_ (toJSVal >=> js_push arr) xs
    return arr

instance {-# OVERLAPPABLE #-} FromJSVal a => FromJSVal [a] where
  fromJSVal v = do
    ok <- js_isArray v
    if ok then do
      n <- js_length v
      sequence <$> mapM (js_index v >=> fromJSVal) [0 .. n - 1]
     else
      return Nothing

instance ToJSVal Int where
  toJSVal = js_fromInt

instance FromJSVal Int where
  fromJSVal = checked js_isNumber js_toInt

instance ToJSVal Word where
  toJSVal = js_fromWord

instance FromJSVal Word where
  fromJSVal = checked js_isNumber js_toWord

instance ToJSVal Double where
  toJSVal = js_fromDouble

instance FromJSVal Double where
  fromJSVal = checked js_isNumber js_toDouble

instance ToJSVal Float where
  toJSVal = js_fromFloat

instance FromJSVal Float where
  fromJSVal = checked js_isNumber js_toFloat

instance ToJSVal Bool where
  toJSVal = js_fromBool

instance FromJSVal Bool where
  fromJSVal = checked js_isBoolean js_toBool

-- A Char is a JavaScript string with one code point.
instance ToJSVal Char where
  toJSVal = js_fromChar

instance FromJSVal Char where
  fromJSVal = checked js_isString js_toChar

instance ToJSVal () where
  toJSVal () = return jsNull

-- Any value, e.g. the undefined of statements without a return.
instance FromJSVal () where
  fromJSVal _ = return (Just ())

instance ToJSVal a => ToJSVal (Maybe a) where
  toJSVal Nothing  = return jsNull
  toJSVal (Just a) = toJSVal a

instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal v = do
    nullish <- js_isNullish v
    if nullish then return (Just Nothing) else fmap Just <$> fromJSVal v
