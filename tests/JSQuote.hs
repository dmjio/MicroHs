module JSQuote(main) where
import Control.Exception
import Data.List(isInfixOf)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Wasm.Marshal
import GHC.Wasm.Prim

-- A quote in a function; the same code runs many times (it is compiled once).
double :: Int -> IO Int
double i = [js| ${i} * 2 |]

main :: IO ()
main = do
  -- an expression with interpolated variables
  let a = 3 :: Int
      b = 4 :: Int
  n <- [js| ${a} + ${b} |] :: IO Int
  print n
  -- no spaces around the quote and the interpolations
  m <- [js|${a}*${b}|] :: IO Int
  print m
  -- statements with return, an interpolated expression, a comment and a string
  s <- [js|
    // sum 1..n
    var s = 0;
    for (var i = 1; i <= ${a * 10}; i++) s += i;
    return 'sum=' + s;
  |] :: IO String
  putStrLn s
  -- no return: the result is undefined, which is ()
  [js| var unused = ${a}; |] :: IO ()
  -- strings, in both directions
  let name = "world"
  greeting <- [js| "hello, " + ${name} + "!" |] :: IO String
  putStrLn greeting
  len <- [js| ${T.pack "text \955"}.length |] :: IO Int
  print len
  t <- [js| ${name}.toUpperCase() |] :: IO Text
  print t
  -- numbers, Bool, Char
  d <- [js| ${1.5 :: Double} * ${2 :: Int} |] :: IO Double
  print d
  bs <- [js| [${True}, !${True}, ${b} > ${a}] |] :: IO [Bool]
  print bs
  c <- [js| ${'x'} + ${'\955'} |] :: IO String
  putStrLn c
  c' <- [js| ${"abc"}[1] |] :: IO Char
  print c'
  -- lists are arrays
  xs <- [js| ${[1, 2, 3 :: Int]}.map(x => x * 10) |] :: IO [Int]
  print xs
  ws <- [js| ${["a", "bb"] :: [String]}.map(w => w.length) |] :: IO [Int]
  print ws
  -- Maybe: null or undefined is Nothing
  ma <- [js| ${Just a} |] :: IO (Maybe Int)
  mb <- [js| ${Nothing :: Maybe Int} |] :: IO (Maybe Int)
  mc <- [js| undefined |] :: IO (Maybe Int)
  print (ma, mb, mc)
  -- JSVal values pass through
  o <- [js| ({ x: ${a}, y: ${name} }) |] :: IO JSVal
  x <- [js| ${o}.x |] :: IO Int
  print x
  [js| JSON.stringify(${o}) |] >>= putStrLn
  -- a value of the wrong type
  str <- [js| "not a number" |] :: IO JSVal
  (fromJSVal str :: IO (Maybe Int)) >>= print
  (fromJSVal str :: IO (Maybe String)) >>= print
  -- the same quote, many times
  mapM_ (\ i -> double i >>= print) [1, 2, 3]
  -- a JavaScript exception is a JSException
  r <- try ([js| ${o}.no.such.thing |] :: IO Int)
  case r of
    Left e@(JSException _) -> putStrLn $ "caught " ++ (if "TypeError" `isInfixOf` show e then "TypeError" else show e)
    Right _ -> putStrLn "no exception"
  [js| console.log("done", ${n}) |] :: IO ()
