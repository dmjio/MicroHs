module GHC.Wasm.Marshal where
import qualified Prelude()
import Primitives(IO, JSVal)
import Data.Char_Type
import Data.Maybe_Type

class ToJSVal a where
  toJSVal :: a -> IO JSVal

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  fromJSValUnchecked :: JSVal -> IO a

jsQuote :: forall a . FromJSVal a => String -> [IO JSVal] -> IO a
