-- Two-level type theory (staging) support.
--
-- MicroHs has two stages: the meta level (compile time) and the object level (run time).
-- Every type and term lives at one stage; the only connection between the stages is
--
--   Code a      the meta level type of object level code of type a
--   [| e |]     quotation:  e :: a (object level)  gives  [| e |] :: Code a (meta level)
--   ~e          splice:     e :: Code a (meta level)  gives  ~e :: a (object level)
--
-- with  ~[| e |] = e  and  [| ~e |] = e.
-- Ordinary definitions that do not mention Code are stage polymorphic and can be
-- used at both stages; the whole Prelude is available at compile time.
-- Meta level definitions are evaluated by the compiler and never exist at run time.
module Staged(
  Code,
  codeInt, codeWord, codeChar, codeDouble, codeFloat, codeString,
  codeBool, codeInteger, codeList,
  Gen(..), runGen, gen, genLet,
  ) where
import Prelude
import Data.Integer(_integerToIntList, _intListToInteger)
import Primitives(Code)

-- Serialization of compile time literals into object level literals.
-- These are the only way a meta level value can be turned into code
-- (two-level type theory has no general cross stage persistence).
codeInt :: Int -> Code Int
codeInt = _primitive "$liftInt"

codeWord :: Word -> Code Word
codeWord = _primitive "$liftInt"

codeChar :: Char -> Code Char
codeChar = _primitive "$liftInt"

codeDouble :: Double -> Code Double
codeDouble = _primitive "$liftDouble"

codeFloat :: Float -> Code Float
codeFloat = _primitive "$liftFloat"

codeString :: String -> Code String
codeString = _primitive "$liftString"

codeBool :: Bool -> Code Bool
codeBool True = [| True |]
codeBool False = [| False |]

codeInteger :: Integer -> Code Integer
codeInteger i = [| _intListToInteger ~(codeList (map codeInt (_integerToIntList i))) |]

-- The code for a list, from the code of its elements.
codeList :: [Code a] -> Code [a]
codeList [] = [| [] |]
codeList (c : cs) = [| ~c : ~(codeList cs) |]

-- The code generation monad: continuation passing over object level code.
-- It is used to insert object level let bindings from meta level code,
-- see Kovács, "Closure-Free Functional Programming in a Two-Level Type Theory".
newtype Gen a = Gen (forall r . (a -> Code r) -> Code r)

unGen :: Gen a -> (a -> Code r) -> Code r
unGen (Gen f) = f

instance Functor Gen where
  fmap f (Gen g) = Gen $ \ k -> g (k . f)

instance Applicative Gen where
  pure a = Gen $ \ k -> k a
  Gen f <*> Gen a = Gen $ \ k -> f (\ g -> a (k . g))

instance Monad Gen where
  Gen g >>= f = Gen $ \ k -> g (\ a -> unGen (f a) k)

runGen :: Gen (Code a) -> Code a
runGen (Gen f) = f id

-- Bind the code to an object level variable, so it is evaluated only once,
-- and return (code for) the variable.
gen :: Code a -> Gen (Code a)
gen c = Gen $ \ k -> [| let x = ~c in ~(k [| x |]) |]

-- Same as gen, but written as a function.
genLet :: Code a -> (Code a -> Code b) -> Code b
genLet c f = [| let x = ~c in ~(f [| x |]) |]
