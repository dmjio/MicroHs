module EmbedFile(main) where
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- $(embedFile "path") reads the file when the module is compiled and
-- embeds its contents as a string literal, so the type decides what you get.

asString :: String
asString = $(embedFile "embed.txt")

asBytes :: BS.ByteString
asBytes = $(embedFile "embed.txt")

asText :: T.Text
asText = $(embedFile "embed.txt")

-- Binary files keep their bytes.
binary :: BS.ByteString
binary = $(embedFile "embed.bin")

-- With no annotation it is a String, like any string literal.
main :: IO ()
main = do
  putStr asString
  print (length asString)                 -- characters (UTF-8 decoded)
  print (BS.length asBytes)               -- bytes
  print (T.length asText)
  print (asText == T.pack asString)
  print (BS.unpack binary)
  putStrLn (lines $(embedFile "embed.txt") !! 0)
