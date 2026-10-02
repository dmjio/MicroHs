-- Two-level type theory: staging.
--
-- After type checking and desugaring, a module contains three kinds of definitions:
--  * meta level definitions (stage LMeta): only exist at compile time.
--    They are kept (in lambda form) in tMetaDefs so that importing modules can run them.
--  * object level definitions (stage LObj): run time code.  They can contain splices
--    (~e, marked by the pseudo primitive "$splice") which are evaluated here.
--  * stage polymorphic definitions: ordinary code, usable at both stages.
--
-- Staging follows Kovács, "Staged Compilation with Two-Level Type Theory" (ICFP 2022),
-- section 4: a meta level evaluator (eval1) producing values, where quoted code is a value,
-- and an object level "evaluator" (eval0) that only renames variables and executes splices.
-- Object level binders are handled with HOAS closures and read back with de Bruijn levels.
module MicroHs.Stage(
  stageModule,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Char(ord, chr, isDigit)
import Data.Int(Int64)
import Data.Word(Word)
import Data.Bits
import Data.List(partition)
import Data.Maybe
import MicroHs.Desugar(LDef, quotePrim, splicePrim, hasSplice)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), Level(..), showLit, HasLoc(..))
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Names(uniqIdentSep)
import MicroHs.TCMonad(LevelTable)
import MicroHs.TypeCheck(TModule, tBindingsOf, tMetaDefs, setBindings, setMetaDefs)
import Text.PrettyPrint.HughesPJLiteClass(prettyShow)

-- Stage a desugared module.
-- Returns the module with the meta level definitions moved to tMetaDefs and all
-- splices in object level definitions executed, together with the list of staged definitions.
stageModule :: LevelTable -> [TModule [LDef]] -> TModule [LDef] -> (TModule [LDef], [LDef])
stageModule levels imported dmdl =
  let defs = tBindingsOf dmdl
      isMeta (i, _) = M.lookup i levels == Just LMeta
      (metas, objs) = partition isMeta defs
      -- Everything a compile time computation might need.
      globals = Globals $ M.fromList
                  [ (i, eval1 globals M.empty e)
                  | (i, e) <- defs ++ concat [ tBindingsOf tm ++ tMetaDefs tm | tm <- imported ] ]
      stage (i, e) | hasSplice e = (i, quote0 0 $ eval0 globals M.empty e)
                   | otherwise   = (i, e)
      objs' = map stage objs
      staged = [ d | (d, (_, e)) <- zip objs' objs, hasSplice e ]
  in  (setMetaDefs (setBindings dmdl objs') metas, staged)

-----------------------------------------------
-- Values

-- Meta level values.
data Val
  = VLam (Val -> Val)        -- functions, and Scott encoded data
  | VInt Int                 -- Int, Word, Char, Int64, Word64
  | VDbl Double
  | VFlt Float
  | VStr String              -- a string literal, behaves as a Scott encoded list
  | VQuote Code              -- quoted object level code
  | VTup Val Val             -- only used internally when extracting lists

-- Object level code, with HOAS binders.
data Code
  = CVar Int Ident           -- de Bruijn level and base name
  | CGlobal Ident
  | CApp Code Code
  | CLam Ident (Code -> Code)
  | CLit Lit

-- Environment entries for local variables.
data Ent = EV Val | EC Code

type Env = M.Map Ent

newtype Globals = Globals (M.Map Val)

stageError :: forall a . String -> a
stageError msg = error $ "staging error: " ++ msg

-----------------------------------------------
-- Meta level evaluation

eval1 :: Globals -> Env -> Exp -> Val
eval1 g env ae =
  case ae of
    Var i ->
      case M.lookup i env of
        Just (EV v) -> v
        Just (EC _) -> stageError $ "object level variable used at compile time: " ++ showIdent i
        Nothing -> globalVal g i
    App (Lit (LPrim p)) e | p == quotePrim -> VQuote (eval0 g env e)
                          | p == splicePrim -> stageError "splice at meta level"
    App f a -> apply (eval1 g env f) (eval1 g env a)
    Lam x e -> VLam $ \ v -> eval1 g (M.insert x (EV v) env) e
    Lit l -> litVal l

globalVal :: Globals -> Ident -> Val
globalVal (Globals m) i =
  case M.lookup i m of
    Just v -> v
    Nothing
      | isIdent "Control.Error._errorLoc" i || isIdent "Control.Error._undefinedLoc" i ->
        VLam $ \ l -> VLam $ \ s -> stageError $ "error called at compile time: " ++ valString l ++ valString s
      | otherwise -> stageError $ "unknown global at compile time: " ++ showIdent i

apply :: Val -> Val -> Val
apply f a =
  case f of
    VLam h -> h a
    VStr "" -> VLam (const a)                                        -- [] n c = n
    VStr (c:cs) -> VLam $ \ k -> apply (apply k (VInt (ord c))) (VStr cs)   -- (x:xs) n c = c x xs
    VInt _ -> stageError "application of an integer"
    VDbl _ -> stageError "application of a double"
    VFlt _ -> stageError "application of a float"
    VQuote _ -> stageError "application of code"
    VTup _ _ -> stageError "application of a tuple"

apply2 :: Val -> Val -> Val -> Val
apply2 f a b = apply (apply f a) b

-- Force a value to weak head normal form.
whnf :: Val -> ()
whnf v =
  case v of
    VLam _ -> ()
    VInt _ -> ()
    VDbl _ -> ()
    VFlt _ -> ()
    VStr _ -> ()
    VQuote _ -> ()
    VTup _ _ -> ()

litVal :: Lit -> Val
litVal l =
  case l of
    LInt i -> VInt i
    LInt64 i -> VInt (fromIntegral i)
    LDouble d -> VDbl d
    LFloat f -> VFlt f
    LChar c -> VInt (ord c)
    LStr s -> VStr s
    LPrim p -> primVal p
    LTick _ -> VLam id
    LForImp _ _ _ _ -> stageError "foreign function called at compile time"
    _ -> stageError $ "literal not supported at compile time: " ++ showLit l

-- Convert a Scott encoded list to a Haskell list.
valList :: Val -> [Val]
valList v =
  case v of
    VStr s -> map (VInt . ord) s
    _ ->
      case apply2 v (VInt 0) (VLam $ \ x -> VLam $ \ xs -> VTup x xs) of
        VTup x xs -> x : valList xs
        _ -> []

valString :: Val -> String
valString = map (chr . valInt) . valList

valInt :: Val -> Int
valInt (VInt i) = i
valInt _ = stageError "integer expected"

valDbl :: Val -> Double
valDbl (VDbl d) = d
valDbl _ = stageError "double expected"

valFlt :: Val -> Float
valFlt (VFlt f) = f
valFlt _ = stageError "float expected"

vBool :: Bool -> Val
vBool False = cK      -- False n c  (encIf c t e = c e t)
vBool True  = cA

vOrdering :: Ordering -> Val
vOrdering LT = cK2
vOrdering EQ = cKK
vOrdering GT = cKA

-- Some Scott encoded constructors
cK, cA, cK2, cKK, cKA :: Val
cK  = VLam $ \ x -> VLam (const x)
cA  = VLam $ const (VLam id)
cK2 = VLam $ \ x -> VLam $ const (VLam (const x))
cKK = VLam $ const (VLam $ \ y -> VLam (const y))
cKA = VLam $ const (VLam $ const (VLam id))

-----------------------------------------------
-- Primitives

primVal :: String -> Val
primVal p =
  case p of
    "S"   -> lam3 $ \ f g x -> apply (apply f x) (apply g x)
    "K"   -> cK
    "I"   -> VLam id
    "B"   -> lam3 $ \ f g x -> apply f (apply g x)
    "C"   -> lam3 $ \ f g x -> apply (apply f x) g
    "S'"  -> lam4 $ \ k f g x -> apply (apply k (apply f x)) (apply g x)
    "B'"  -> lam4 $ \ k f g x -> apply (apply k f) (apply g x)
    "C'"  -> lam4 $ \ k f g x -> apply (apply k (apply f x)) g
    "A"   -> cA
    "U"   -> lam2 $ \ x y -> apply y x
    "Y"   -> VLam $ \ f -> let r = apply f r in r
    "Z"   -> lam3 $ \ f g _ -> apply f g
    "J"   -> lam3 $ \ x _ z -> apply z x
    "P"   -> lam3 $ \ x y f -> apply2 f x y
    "R"   -> lam3 $ \ x y f -> apply2 y f x
    "O"   -> lam4 $ \ x y _ f -> apply2 f x y
    "L"   -> lam3 $ \ x f _ -> apply f x
    "K2"  -> cK2
    "KK"  -> cKK
    "KA"  -> cKA
    "K3"  -> lam4 $ \ x _ _ _ -> x
    "K4"  -> lam4 $ \ x _ _ _ -> VLam (const x)
    "C'B" -> lam4 $ \ x y z w -> apply (apply x z) (apply y w)
    'T':ds | Just n <- readIntMaybe ds -> tupleCon n
    'T':'A':'G':ds | Just n <- readIntMaybe ds -> VLam $ \ x -> VLam $ \ f -> apply2 f (VInt n) x

    -- Int (and Word, Char, Int64, Word64)
    "+" -> arith (+)
    "-" -> arith (-)
    "*" -> arith (*)
    "quot" -> arith quot
    "rem" -> arith rem
    "subtract" -> arith subtract
    "neg" -> arith1 negate
    "inv" -> arith1 complement
    "u+" -> arith (+)
    "u-" -> arith (-)
    "u*" -> arith (*)
    "usubtract" -> arith subtract
    "uneg" -> arith1 negate
    "uquot" -> arithW quot
    "urem" -> arithW rem
    "and" -> arith (.&.)
    "or" -> arith (.|.)
    "xor" -> arith xor
    "shl" -> arith shiftL
    "shr" -> lam2 $ \ x y -> VInt (fromIntegral (shiftR (toWord (valInt x)) (valInt y)))
    "ashr" -> arith shiftR
    "popcount" -> arith1 popCount
    "clz" -> arith1 (countLeadingZeros . toWord)
    "ctz" -> arith1 (countTrailingZeros . toWord)
    "==" -> cmpI (==)
    "/=" -> cmpI (/=)
    "<"  -> cmpI (<)
    "<=" -> cmpI (<=)
    ">"  -> cmpI (>)
    ">=" -> cmpI (>=)
    "u<"  -> cmpW (<)
    "u<=" -> cmpW (<=)
    "u>"  -> cmpW (>)
    "u>=" -> cmpW (>=)
    "icmp" -> lam2 $ \ x y -> vOrdering (compare (valInt x) (valInt y))
    "ucmp" -> lam2 $ \ x y -> vOrdering (compare (toWord (valInt x)) (toWord (valInt y)))
    'I':q | q `elem` ["+","-","*","quot","rem","subtract","neg","inv","u+","u-","u*","usubtract","uneg","uquot","urem",
                      "and","or","xor","shl","shr","ashr","popcount","clz","ctz","==","/=","<","<=",">",">=",
                      "u<","u<=","u>","u>=","icmp","ucmp"] -> primVal q
    "itoI" -> VLam id
    "Itoi" -> VLam id
    "utoU" -> VLam id
    "Utou" -> VLam id
    "ord" -> VLam id
    "chr" -> VLam id

    -- Double
    "d+" -> darith (+)
    "d-" -> darith (-)
    "d*" -> darith (*)
    "d/" -> darith (/)
    "dneg" -> VLam $ \ x -> VDbl (negate (valDbl x))
    "d==" -> dcmp (==)
    "d/=" -> dcmp (/=)
    "d<"  -> dcmp (<)
    "d<=" -> dcmp (<=)
    "d>"  -> dcmp (>)
    "d>=" -> dcmp (>=)
    "itod" -> VLam $ \ x -> VDbl (fromIntegral (valInt x))
    "Itod" -> VLam $ \ x -> VDbl (fromIntegral (valInt x))
    "utod" -> VLam $ \ x -> VDbl (fromIntegral (toWord (valInt x)))
    "dtoi" -> VLam $ \ x -> VInt (truncate (valDbl x))
    "dtof" -> VLam $ \ x -> VFlt (realToFrac (valDbl x))
    "ftod" -> VLam $ \ x -> VDbl (realToFrac (valFlt x))

    -- Float
    "f+" -> farith (+)
    "f-" -> farith (-)
    "f*" -> farith (*)
    "f/" -> farith (/)
    "fneg" -> VLam $ \ x -> VFlt (negate (valFlt x))
    "f==" -> fcmp (==)
    "f/=" -> fcmp (/=)
    "f<"  -> fcmp (<)
    "f<=" -> fcmp (<=)
    "f>"  -> fcmp (>)
    "f>=" -> fcmp (>=)
    "itof" -> VLam $ \ x -> VFlt (fromIntegral (valInt x))
    "Itof" -> VLam $ \ x -> VFlt (fromIntegral (valInt x))
    "utof" -> VLam $ \ x -> VFlt (fromIntegral (toWord (valInt x)))
    "ftoi" -> VLam $ \ x -> VInt (truncate (valFlt x))

    -- Cross stage persistence of literals (Staged.codeInt etc)
    "$liftInt" -> VLam $ \ x -> VQuote (CLit (LInt (valInt x)))
    "$liftDouble" -> VLam $ \ x -> VQuote (CLit (LDouble (valDbl x)))
    "$liftFloat" -> VLam $ \ x -> VQuote (CLit (LFloat (valFlt x)))
    "$liftString" -> VLam $ \ x -> VQuote (CLit (LStr (valString x)))

    "seq" -> lam2 $ \ a b -> whnf a `seq` b
    "rnf" -> lam2 $ \ a b -> whnf a `seq` b    -- XXX only weak head normal form
    "raise" -> VLam $ \ _ -> stageError "uncaught exception at compile time"
    "tick" -> VLam id

    _ -> stageError $ "primitive not available at compile time: " ++ p
  where
    lam2 f = VLam $ \ a -> VLam $ \ b -> f a b
    lam3 f = VLam $ \ a -> VLam $ \ b -> VLam $ \ c -> f a b c
    lam4 f = VLam $ \ a -> VLam $ \ b -> VLam $ \ c -> VLam $ \ d -> f a b c d
    arith op = lam2 $ \ x y -> VInt (op (valInt x) (valInt y))
    arith1 op = VLam $ \ x -> VInt (op (valInt x))
    arithW op = lam2 $ \ x y -> VInt (fromIntegral (op (toWord (valInt x)) (fromIntegral (valInt y) :: Word)))
    cmpI op = lam2 $ \ x y -> vBool (op (valInt x) (valInt y))
    cmpW op = lam2 $ \ x y -> vBool (op (toWord (valInt x)) (toWord (valInt y)))
    darith op = lam2 $ \ x y -> VDbl (op (valDbl x) (valDbl y))
    dcmp op = lam2 $ \ x y -> vBool (op (valDbl x) (valDbl y))
    farith op = lam2 $ \ x y -> VFlt (op (valFlt x) (valFlt y))
    fcmp op = lam2 $ \ x y -> vBool (op (valFlt x) (valFlt y))
    -- T_n x1 ... xn f = f x1 ... xn
    tupleCon n = go n []
      where go 0 xs = VLam $ \ f -> foldl apply f (reverse xs)
            go k xs = VLam $ \ x -> go (k - 1 :: Int) (x : xs)

toWord :: Int -> Word
toWord = fromIntegral

readIntMaybe :: String -> Maybe Int
readIntMaybe s | not (null s) && all isDigit s = Just (foldl (\ a c -> a * 10 + ord c - ord '0') 0 s)
               | otherwise = Nothing

-----------------------------------------------
-- Object level evaluation: rename variables, execute splices.

eval0 :: Globals -> Env -> Exp -> Code
eval0 g env ae =
  case ae of
    Var i ->
      case M.lookup i env of
        Just (EC c) -> c
        Just (EV _) -> stageError $ "meta level variable used in object code: " ++ showIdent i
        Nothing -> CGlobal i
    App (Lit (LPrim p)) e | p == splicePrim ->
                              case eval1 g env e of
                                VQuote c -> c
                                _ -> stageError "splice of a non-code value"
                          | p == quotePrim -> stageError "quotation at object level"
    App f a -> CApp (eval0 g env f) (eval0 g env a)
    Lam x e -> CLam x $ \ c -> eval0 g (M.insert x (EC c) env) e
    Lit l -> CLit l

-- Read back object level code to an expression.
-- Binders are named after the original variable and the de Bruijn level.
quote0 :: Int -> Code -> Exp
quote0 d ac =
  case ac of
    CVar l x -> Var (lvlIdent l x)
    CGlobal i -> Var i
    CApp f a -> App (quote0 d f) (quote0 d a)
    CLam x f -> Lam (lvlIdent d x) (quote0 (d + 1) (f (CVar d x)))
    CLit l -> Lit l

lvlIdent :: Int -> Ident -> Ident
lvlIdent l x = mkIdentSLoc (getSLoc x) (unIdent x ++ uniqIdentSep ++ "s" ++ show l)

-- Keep the pretty printer import used (for debugging aids).
_showCode :: Exp -> String
_showCode = prettyShow
