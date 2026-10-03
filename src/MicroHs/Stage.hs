-- Two-level type theory: staging.
--
-- After type checking and desugaring, a module contains three kinds of definitions:
--  * meta level definitions (stage LMeta): only exist at compile time.
--    They are kept (as combinators) in tMetaDefs so that importing modules can run them.
--  * object level definitions (stage LObj): run time code.  They can contain splices
--    (~e, marked by the pseudo primitive "$splice") which are evaluated here.
--  * stage polymorphic definitions: ordinary code, usable at both stages.
--
-- Staging follows Kovács, "Staged Compilation with Two-Level Type Theory" (ICFP 2022),
-- section 4, with the meta level evaluator being the ordinary runtime system:
--  * Meta level code is compiled to combinators and run by the runtime system, in the
--    same way as the interactive system runs code (MicroHs.Translate).  So compile time
--    code has exactly the run time semantics, and can use everything the runtime can
--    (e.g., the FFI).  A quotation is compiled to code that builds a value of type Code,
--    with the object level binders as functions (HOAS).
--  * The object level "evaluator" (eval0) runs in the compiler.  It only renames
--    variables and executes splices, and the result is read back (quote0) with
--    de Bruijn levels, so generated code never captures variables.
module MicroHs.Stage(
  stageModule,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Int(Int64)
import Data.List(partition, nub)
import Data.Maybe
import Unsafe.Coerce(unsafeCoerce)
import MicroHs.Abstract(compileOpt)
import MicroHs.Desugar(LDef, quotePrim, splicePrim, hasSplice)
import MicroHs.EncodeData(encConstr, encList)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), Level(..), showLit, HasLoc(..), errorMessage)
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Names(uniqIdentSep)
import MicroHs.State
import MicroHs.TCMonad(LevelTable)
import MicroHs.Translate(translateMap, translateWithMap)
import MicroHs.TypeCheck(TModule, tBindingsOf, tMetaDefs, setBindings, setMetaDefs, impossible)

-- Stage a desugared module.
-- Returns the module with the meta level definitions moved to tMetaDefs and all
-- splices in object level definitions executed, together with the list of staged definitions,
-- and the compiled meta level code of the module.  Forcing the latter finds the staging
-- errors in the module; forcing a staged definition runs its splices.
stageModule :: LevelTable -> [TModule [LDef]] -> TModule [LDef] -> (TModule [LDef], [LDef], [Exp])
stageModule levels imported dmdl =
  let defs = tBindingsOf dmdl
      isMeta (i, _) = M.lookup i levels == Just LMeta
      (metas, objs) = partition isMeta defs
      -- Meta level definitions: compile the quotations and turn them into combinators.
      metas' = [ (i, compileOpt (metaExp (getSLoc i) M.empty e)) | (i, e) <- metas ]
      -- Object level definitions: take out the splices.
      (objs', (_, rsplices)) = runState (mapM prepDef objs) (0, [])
      -- Everything a compile time computation might need:
      -- the imported modules, and the definitions of this module that have no splices.
      tmap = translateMap $ codeConstrs ++ concat [ tBindingsOf tm ++ tMetaDefs tm | tm <- imported ]
      here = metas' ++ [ (i, compileOpt e) | (i, e) <- objs, not (hasSplice e) ]
      -- All the splices of the module are loaded into the runtime together.
      fns :: [SpliceFn]
      fns = unsafeCoerce $ translateWithMap tmap (here, compileOpt (encList (reverse rsplices)))
      stage (i, e) | hasSplice e = (i, quote0 0 $ eval0 fns M.empty e)
                   | otherwise   = (i, e)
      objs'' = map stage objs'
      staged = [ d | (d, (_, e)) <- zip objs'' objs, hasSplice e ]
      noRun (i, _) = errorMessage (getSLoc i)
                       "splices are run by the MicroHs runtime, so they need a compiler that is compiled with mhs"
  in  case staged of
        d : _ | not compiledWithMhs -> noRun d
        _ -> (setMetaDefs (setBindings dmdl objs'') metas', staged, map snd metas' ++ rsplices)

stageError :: forall a . SLoc -> String -> a
stageError loc msg = errorMessage loc $ "staging error: " ++ msg

-----------------------------------------------
-- Object level code

-- Object level code, with HOAS binders.
-- Values of this type are also built by meta level code running in the runtime system.
-- There is no source definition of the type for that code, instead the constructor
-- functions are generated here (codeConstrs), with the encoding the compiler itself uses.
-- So the constructors below, their order, and their arities must agree with codeConTable.
data Code
  = CVar Int String              -- de Bruijn level and base name; only made by quote0
  | CExp Exp                     -- a global variable or a literal; only made by eval0
  | CGlobal String               -- global variable
  | CApp Code Code
  | CLam String (Code -> Code)
  | CInt Int                     -- literals
  | CInt64 Int64
  | CDbl Double
  | CFlt Float
  | CChr Char
  | CStr String
  | CPrim String

-- Name and arity of the constructors of Code, in order.
codeConTable :: [(String, Int)]
codeConTable =
  [ ("var", 2), ("exp", 1), ("global", 1), ("app", 2), ("lam", 2)
  , ("int", 1), ("int64", 1), ("dbl", 1), ("flt", 1), ("chr", 1), ("str", 1), ("prim", 1) ]

-- The constructor functions of Code for the code run by the runtime system.
codeConstrs :: [LDef]
codeConstrs = [ (codeCon s, compileOpt (encConstr i n (replicate a False))) | (i, (s, a)) <- zip [0..] codeConTable ]
  where n = length codeConTable

codeCon :: String -> Ident
codeCon s = mkIdent ("$Code." ++ s)

con1 :: String -> Exp -> Exp
con1 s = App (Var (codeCon s))

con2 :: String -> Exp -> Exp -> Exp
con2 s a = App (App (Var (codeCon s)) a)

-----------------------------------------------
-- Compiling meta level code

-- How a local variable is bound.
data Bind
  = BMeta                -- meta level variable
  | BObj (Maybe Exp)     -- object level variable (it is bound to Code when in meta level code).
                         -- When it is let bound to a expression that does not depend on the
                         -- object level, that expression is also its value at the meta level.
                         -- (This happens for the dictionary bindings the type checker inserts.)

type BEnv = M.Map Bind

-- Compile meta level code: quotations become code that builds a Code value.
metaExp :: SLoc -> BEnv -> Exp -> Exp
metaExp loc env ae =
  case ae of
    Var i ->
      case M.lookup i env of
        Just (BObj (Just e)) -> e
        Just (BObj Nothing) -> stageError loc $ "object level variable used at compile time: " ++ showIdent i
        _ -> ae
    App (Lit (LPrim p)) e | p == quotePrim -> quoteExp loc env e
                          | p == splicePrim -> stageError loc "splice at meta level"
    App f a -> App (metaExp loc env f) (metaExp loc env a)
    Lam x e -> Lam x (metaExp loc (M.insert x BMeta env) e)
    -- Cross stage persistence of literals (Staged.codeInt etc)
    Lit (LPrim "$liftInt") -> Var (codeCon "int")
    Lit (LPrim "$liftDouble") -> Var (codeCon "dbl")
    Lit (LPrim "$liftFloat") -> Var (codeCon "flt")
    Lit (LPrim "$liftString") -> Var (codeCon "str")
    Lit _ -> ae

-- Compile quoted object level code to meta level code that builds it.
-- Object level binders become meta level functions on Code.
quoteExp :: SLoc -> BEnv -> Exp -> Exp
quoteExp loc env ae =
  case ae of
    Var i ->
      case M.lookup i env of
        Nothing -> con1 "global" (Lit (LStr (unIdent i)))
        Just (BObj _) -> ae
        Just BMeta -> stageError loc $ "meta level variable used in object code: " ++ showIdent i
    App (Lit (LPrim p)) e | p == splicePrim -> metaExp loc env e
                          | p == quotePrim -> stageError loc "quotation at object level"
    App (Lam x b) a -> con2 "app" (lam x (BObj (metaView env a)) b) (quoteExp loc env a)
    App f a -> con2 "app" (quoteExp loc env f) (quoteExp loc env a)
    Lam x e -> lam x (BObj Nothing) e
    Lit l ->
      case l of
        LInt _    -> con1 "int" ae
        LInt64 _  -> con1 "int64" ae
        LDouble _ -> con1 "dbl" ae
        LFloat _  -> con1 "flt" ae
        LChar _   -> con1 "chr" ae
        LStr _    -> con1 "str" ae
        LPrim p   -> con1 "prim" (Lit (LStr p))
        _ -> stageError loc $ "literal not supported in a quotation: " ++ showLit l
  where lam x b e = con2 "lam" (Lit (LStr (unIdent x))) (Lam x (quoteExp loc (M.insert x b env) e))

-- The value of an object level expression at the meta level,
-- if it does not depend on anything at the object level.
metaView :: BEnv -> Exp -> Maybe Exp
metaView env ae =
  case ae of
    Var i ->
      case M.lookup i env of
        Just (BObj m) -> m
        _ -> Just ae
    App f a -> App <$> metaView env f <*> metaView env a
    Lam x e -> Lam x <$> metaView (M.insert x BMeta env) e
    Lit (LPrim p) | p == quotePrim || p == splicePrim -> Nothing
    Lit _ -> Just ae

-----------------------------------------------
-- Taking out the splices

-- Replace each splice ~e in an object level definition by
--   $splice n x1 ... xk
-- where n is the number of the splice and x1 ... xk are the object level variables in e.
-- Splice number n is the meta level function \ x1 ... xk -> e.
prepDef :: LDef -> State (Int, [Exp]) LDef
prepDef (i, e) | hasSplice e = (,) i <$> prepExp (getSLoc i) M.empty e
               | otherwise   = return (i, e)

prepExp :: SLoc -> BEnv -> Exp -> State (Int, [Exp]) Exp
prepExp loc env ae =
  case ae of
    App (Lit (LPrim p)) e
      | p == splicePrim -> do
          let e' = metaExp loc env e
              xs = nub [ x | x <- freeVars e', isJust (M.lookup x env) ]
          (n, ss) <- get
          put (n + 1, lams xs e' : ss)
          return $ App (Lit (LPrim splicePrim)) (apps (Lit (LInt n)) (map Var xs))
      | p == quotePrim -> stageError loc "quotation at object level"
    App (Lam x b) a -> do
      a' <- prepExp loc env a
      b' <- prepExp loc (M.insert x (BObj (metaView env a)) env) b
      return (App (Lam x b') a')
    App f a -> App <$> prepExp loc env f <*> prepExp loc env a
    Lam x e -> Lam x <$> prepExp loc (M.insert x (BObj Nothing) env) e
    _ -> return ae

-----------------------------------------------
-- Object level evaluation: rename variables, execute splices.

-- The splice functions are in the first argument.
eval0 :: [SpliceFn] -> M.Map Code -> Exp -> Code
eval0 fns env ae =
  case ae of
    Var i -> fromMaybe (CExp ae) (M.lookup i env)
    App (Lit (LPrim p)) e | p == splicePrim ->
      case getApp e of
        (Lit (LInt n), as) -> runSplice (fns !! n) (map (eval0 fns env) as)
        _ -> impossible
    App f a -> CApp (eval0 fns env f) (eval0 fns env a)
    Lam x e -> CLam (unIdent x) $ \ c -> eval0 fns (M.insert x c env) e
    Lit _ -> CExp ae
  where
    getApp (App f a) = case getApp f of (h, as) -> (h, as ++ [a])
    getApp e = (e, [])

-- Run a splice function (a value in the runtime system) on the code of its variables.
runSplice :: SpliceFn -> [Code] -> Code
runSplice f [] = unsafeCoerce f
runSplice f (c : cs) = runSplice ((unsafeCoerce f :: Code -> SpliceFn) c) cs

-- A splice function in the runtime system; it takes some Code arguments and returns Code.
data SpliceFn

-- Read back object level code to an expression.
-- Binders are named after the original variable and the de Bruijn level.
quote0 :: Int -> Code -> Exp
quote0 d ac =
  case ac of
    CVar l x -> Var (lvlIdent l x)
    CExp e -> e
    CGlobal i -> Var (mkIdent i)
    CApp f a -> App (quote0 d f) (quote0 d a)
    CLam x f | isDummyIdent (mkIdent x) -> Lam dummyIdent (quote0 (d + 1) (f (CVar d x)))
             | otherwise -> Lam (lvlIdent d x) (quote0 (d + 1) (f (CVar d x)))
    CInt i -> Lit (LInt i)
    CInt64 i -> Lit (LInt64 i)
    CDbl x -> Lit (LDouble x)
    CFlt x -> Lit (LFloat x)
    CChr c -> Lit (LChar c)
    CStr s -> Lit (LStr s)
    CPrim p -> Lit (LPrim p)

lvlIdent :: Int -> String -> Ident
lvlIdent l x = mkIdent (x ++ uniqIdentSep ++ "s" ++ show l)
