{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  module MicroHs.TCMonad,
  get, put, gets, modify,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Fail
import Data.Functor
import Control.Monad(when, unless)
import Data.List(nub)
import Data.Maybe(fromMaybe)
import MicroHs.Expr
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import qualified MicroHs.IntMap as IM
import MicroHs.Names
import MicroHs.State
import MicroHs.SymTab
import System.IO.Unsafe(unsafePerformIO)
import Unsafe.Coerce
import Debug.Trace

-----------------------------------------------
-- TC

type TC s a = State s a

tcRun :: forall s a . TC s a -> s -> (a, s)
tcRun = runState

tcError :: forall s a .
           HasCallStack =>
           SLoc -> String -> TC s a
tcError = errorMessage

instance MonadFail Identity where fail = error

tcTrace :: String -> TC s ()
--tcTrace _ = return ()
tcTrace msg = do
  s <- get
  let s' = trace msg s
  seq s' (put s')

-- trace to stdout
tcTrace' :: String -> TC s ()
--tcTrace _ = return ()
tcTrace' msg = do
  s <- get
  let s' = unsafePerformIO $ do putStrLn msg; return s
  seq s' (put s')

-- Run the action, but don't update the state
noEffect :: TC s a -> TC s a
noEffect tca = do
  s <- get
  a <- tca
  put s
  return a

-----------------------------------------------

data TypeExport = TypeExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  [ValueExport]   -- associated values, i.e., constructors, selectors, methods
  deriving (Show)

instance NFData TypeExport where
  rnf (TypeExport a b c) = rnf a `seq` rnf b `seq` rnf c

data ValueExport = ValueExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  deriving (Show)

instance NFData ValueExport where
  rnf (ValueExport a b) = rnf a `seq` rnf b

-----------------------------------------------
-- Tables

type ValueTable = SymTab           -- type of value identifiers, used during type checking values
type TypeTable  = SymTab           -- kind of type  identifiers, used during kind checking types
type KindTable  = SymTab           -- sort of kind  identifiers, used during sort checking kinds
type SynTable   = M.Map EType      -- body of type synonyms
type DataTable  = M.Map EDef       -- data/newtype definitions (only used for standalone deriving)
type FixTable   = M.Map Fixity     -- precedence and associativity of operators
type AssocTable = M.Map [ValueExport] -- maps a type identifier to its associated constructors/selectors/methods
type ClassTable = M.Map ClassInfo  -- maps a class identifier to its associated information
type InstTable  = M.Map InstInfo   -- indexed by class name
type MetaTable  = [(Ident, EConstraint)]  -- instances with unification variables
type Constraints= [(Ident, EConstraint)]
type ArgDicts   = [(Ident, EConstraint)]  -- dictionary arguments
type Defaults   = M.Map [EType]    -- defaults, maps from class name to types

-- To make type checking fast it is essential to solve constraints fast.
-- The naive implementation of InstInfo would be [InstDict], but
-- that is slow.
-- Instead, the data structure is specialized
--  * For single parameter type classes for atomic types, e.g., Eq Int
--    we use the type name (i.e., Int) to index into a map that gives
--    the dictionary directly.  This map is also used for dictionary arguments
--    of type, e.g., Eq a.
--  * NOT IMPLEMENTED: look up by type name of the left-most type
--  * As a last resort, just look through dictionaries.
data InstInfo = InstInfo
       (M.Map Expr)               -- map for direct lookup of atomic types
       [InstDict]                 -- slow path
       [IFunDep]
  deriving (Show)

instance NFData InstInfo where
  rnf (InstInfo a b c) = rnf a `seq` rnf b `seq` rnf c

-- This is the dictionary expression, instance variables, instance context,
-- and instance.
type InstDictC  = (Expr, [IdKind], [EConstraint], EConstraint, [IFunDep])
-- This is the dictionary expression, instance context, and types.
-- An instance (C T1 ... Tn) has the type list [T1,...,Tn]
-- The types and constraint can be instantiated by providing a starting TRef
data InstDict   = InstDict Expr (TRef -> ([EConstraint], [EType]))

instance NFData InstDict where
  rnf (InstDict e f) = rnf e `seq` rnf (f 0)

instance Show InstDict where
  showsPrec p (InstDict e f) =
    showParen (p > 10) $ showsPrec 11 e . showChar ' ' . showParen True (showString "\\999->" . shows (f 999))

-- All known type equalities, normalized into a substitution.
type TypeEqTable = [(Ident, EType)]

data ClassInfo = ClassInfo
  [IdKind]         -- class tyvars
  [EConstraint]    -- superclasses
  EType            -- class constructor type
  [(Ident,EType)]  -- methods with their types
  [IFunDep]        -- fundeps
  deriving (Show)
type IFunDep = ([Bool], [Bool])           -- invariant: the length of the lists is the number of class tyvars

instance NFData ClassInfo where
  rnf (ClassInfo a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

-----------------------------------------------
-- TCState
data TCState = TC {
  moduleName  :: IdentModule,           -- current module name
  unique      :: Int,                   -- unique number
  fixTable    :: FixTable,              -- fixities, indexed by QIdent
  typeTable   :: TypeTable,             -- type symbol table
  synTable    :: SynTable,              -- synonyms, indexed by QIdent
  dataTable   :: DataTable,             -- data/newtype definitions
  valueTable  :: ValueTable,            -- value symbol table
  assocTable  :: AssocTable,            -- values associated with a type, indexed by QIdent
  uvarSubst   :: IM.IntMap EType,       -- mapping from unique id to type
  tcMode      :: TCMode,                -- pattern, value, or type
  classTable  :: ClassTable,            -- class info, indexed by QIdent
  ctxTables   :: (InstTable,            -- instances
                  MetaTable,            -- instances with unification variables
                  TypeEqTable,          -- type equalities
                  ArgDicts              -- dictionary arguments
                 ),
  constraints :: Constraints,           -- constraints that have to be solved
  defaults    :: Defaults,              -- current defaults
  stage       :: StageState             -- two-level type theory (staging) state
  }
  deriving (Show)

-- The staging state is kept in a separate record, so that the (very frequent)
-- updates of the other fields of TCState do not get more expensive.
data StageState = StageState {
  ssCurLevel    :: Level,                 -- stage of the expression being checked
  ssLevelSubst  :: IM.IntMap Level,       -- solved level variables (not reset by tcReset)
  ssLevelTable  :: LevelTable,            -- stage of each global value identifier (qualified)
  ssLocalLevels :: LevelTable,            -- stage of each local variable
  ssTypeLevels  :: TypeLevelTable,        -- stage signature of each type constructor and class
  ssDictUses    :: M.Map [Level],         -- the stages at which each dictionary identifier is used
  ssDictChecks  :: [(SLoc, Ident, [Level])],  -- deferred stage checks of uses of global values and dictionaries
  ssStagedNodes :: Bool,                  -- were any EStaged nodes created for the current definition
  ssLevelFixes  :: Int                    -- number of level variables bound so far
  }
  deriving (Show)

curLevel :: TCState -> Level
curLevel = ssCurLevel . stage
levelSubst :: TCState -> IM.IntMap Level
levelSubst = ssLevelSubst . stage
levelTable :: TCState -> LevelTable
levelTable = ssLevelTable . stage
localLevels :: TCState -> LevelTable
localLevels = ssLocalLevels . stage
typeLevels :: TCState -> TypeLevelTable
typeLevels = ssTypeLevels . stage
dictUses :: TCState -> M.Map [Level]
dictUses = ssDictUses . stage
dictChecks :: TCState -> [(SLoc, Ident, [Level])]
dictChecks = ssDictChecks . stage
stagedNodes :: TCState -> Bool
stagedNodes = ssStagedNodes . stage
levelFixes :: TCState -> Int
levelFixes = ssLevelFixes . stage

modifyStage :: (StageState -> StageState) -> T ()
modifyStage f = modify $ \ ts -> ts{ stage = f (stage ts) }

type LevelTable = M.Map Level           -- stage of value identifiers; LPoly means any stage

-- The stage signature of a type constructor: the stage of (T a1 ... an)
-- and the stages of the arguments a1 ... an.
-- An LVar in a signature is schematic, i.e., instantiated with a fresh variable at each use.
type LevelSig = (Level, [Level])
type TypeLevelTable = M.Map LevelSig

-- Hack to avoid cricular module reference.
-- See comment for SetTCState in Expr
tcStateToXTCState :: TCState -> XTCState
tcStateToXTCState = unsafeCoerce
xTCStateToTCState :: XTCState -> TCState
xTCStateToTCState = unsafeCoerce

instTable :: TCState -> InstTable
instTable tc = case ctxTables tc of (x,_,_,_) -> x

metaTable :: TCState -> MetaTable
metaTable tc = case ctxTables tc of (_,x,_,_) -> x

typeEqTable :: TCState -> TypeEqTable
typeEqTable tc = case ctxTables tc of (_,_,x,_) -> x

argDicts :: TCState -> ArgDicts
argDicts tc = case ctxTables tc of (_,_,_,x) -> x


putValueTable :: ValueTable -> T ()
putValueTable venv = modify $ \ ts -> ts{ valueTable = venv }

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = modify $ \ ts -> ts{ typeTable = tenv }

putSynTable :: SynTable -> T ()
putSynTable senv = modify $ \ ts -> ts{ synTable = senv }

putDataTable :: DataTable -> T ()
putDataTable denv = modify $ \ ts -> ts{ dataTable = denv }

putUvarSubst :: IM.IntMap EType -> T ()
putUvarSubst sub = modify $ \ ts -> ts{ uvarSubst = sub }

putTCMode :: TCMode -> T ()
putTCMode m = modify $ \ ts -> ts{ tcMode = m }

putInstTable :: InstTable -> T ()
putInstTable is = do
  (_,ms,eqs,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putMetaTable :: MetaTable -> T ()
putMetaTable ms = do
  (is,_,eqs,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putTypeEqTable :: TypeEqTable -> T ()
putTypeEqTable eqs = do
  (is,ms,_,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putArgDicts :: ArgDicts -> T ()
putArgDicts ads = do
  (is,ms,eqs,_) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putCtxTables :: (InstTable, MetaTable, TypeEqTable, ArgDicts) -> T ()
putCtxTables ct = modify $ \ ts -> ts{ ctxTables = ct }

putConstraints :: Constraints -> T ()
putConstraints es = modify $ \ ts -> ts{ constraints = es }

putDefaults :: Defaults -> T ()
putDefaults ds = modify $ \ ts -> ts{ defaults = ds }


type TRef = Int

-----------------------------------------------
-- Levels (stages)

putCurLevel :: Level -> T ()
putCurLevel l = modifyStage $ \ ss -> ss{ ssCurLevel = l }

-- Run an action with a given current stage.
withLevel :: forall a . Level -> T a -> T a
withLevel l ta = do
  o <- gets curLevel
  putCurLevel l
  a <- ta
  putCurLevel o
  return a

newLevelVar :: T Level
newLevelVar = LVar <$> newUniq

-- Find the representative of a level.
derefLevel :: Level -> T Level
derefLevel l@(LVar n) = do
  m <- gets levelSubst
  case IM.lookup n m of
    Nothing -> return l
    Just l' -> do
      l'' <- derefLevel l'
      when (l'' /= l') $   -- path compression
        modifyStage $ \ ss -> ss{ ssLevelSubst = IM.insert n l'' (ssLevelSubst ss) }
      return l''
derefLevel l = return l

-- Unify two levels.  Returns False if they are different constants.
unifyLevelM :: Level -> Level -> T Bool
unifyLevelM a b = do
  a' <- derefLevel a
  b' <- derefLevel b
  case (a', b') of
    _ | a' == b' -> return True
    (LVar n, _) -> do { modifyStage $ \ ss -> ss{ ssLevelSubst = IM.insert n b' (ssLevelSubst ss), ssLevelFixes = ssLevelFixes ss + 1 }; return True }
    (_, LVar n) -> do { modifyStage $ \ ss -> ss{ ssLevelSubst = IM.insert n a' (ssLevelSubst ss), ssLevelFixes = ssLevelFixes ss + 1 }; return True }
    (LPoly, _) -> return True     -- should not happen, but be lenient
    (_, LPoly) -> return True
    _ -> return False

unifyLevel :: HasCallStack => SLoc -> String -> Level -> Level -> T ()
unifyLevel loc msg a b = do
  ok <- unifyLevelM a b
  unless ok $ do
    a' <- derefLevel a
    b' <- derefLevel b
    tcError loc $ "stage mismatch: " ++ msg ++ " is " ++ showLevel a' ++ " level, but is used at " ++ showLevel b' ++ " level"

-- Make a (possibly polymorphic) table level usable: LPoly gives a fresh variable.
instLevel :: Level -> T Level
instLevel LPoly = newLevelVar
instLevel l = derefLevel l

lookupLevelTable :: Ident -> T (Maybe Level)
lookupLevelTable i = gets (M.lookup i . levelTable)

-- Look up the stage of a variable, local variables first.
lookupLevel :: Ident -> T (Maybe Level)
lookupLevel i = do
  ml <- gets (M.lookup i . localLevels)
  case ml of
    Just _ -> return ml
    Nothing -> lookupLevelTable i

addLocalLevel :: Ident -> Level -> T ()
addLocalLevel i l = modifyStage $ \ ss -> ss{ ssLocalLevels = M.insert i l (ssLocalLevels ss) }

putLocalLevels :: LevelTable -> T ()
putLocalLevels lt = modifyStage $ \ ss -> ss{ ssLocalLevels = lt }

addLevelTable :: Ident -> Level -> T ()
addLevelTable i l = modifyStage $ \ ss -> ss{ ssLevelTable = M.insert i l (ssLevelTable ss) }

putLevelTable :: LevelTable -> T ()
putLevelTable lt = modifyStage $ \ ss -> ss{ ssLevelTable = lt }

lookupTypeLevel :: Ident -> T (Maybe LevelSig)
lookupTypeLevel i = gets (M.lookup i . typeLevels)

addTypeLevel :: Ident -> LevelSig -> T ()
addTypeLevel i s = modifyStage $ \ ss -> ss{ ssTypeLevels = M.insert i s (ssTypeLevels ss) }

-- Record that dictionary d is used at the current stage.
addDictUse :: Ident -> T ()
addDictUse d = do
  modifyStage $ \ ss -> ss{ ssDictUses = M.insertWith (++) d [ssCurLevel ss] (ssDictUses ss) }

addDictUses :: Ident -> [Level] -> T ()
addDictUses d ls = modifyStage $ \ ss -> ss{ ssDictUses = M.insertWith (++) d ls (ssDictUses ss) }

getDictUses :: Ident -> T [Level]
getDictUses d = gets (fromMaybe [] . M.lookup d . dictUses)

addDictCheck :: SLoc -> Ident -> [Level] -> T ()
addDictCheck loc d ls = modifyStage $ \ ss -> ss{ ssDictChecks = (loc, d, ls) : ssDictChecks ss }

newUniq :: T TRef
newUniq = do
  ts <- get
  let n' = n + 1
      n = unique ts
  put $ seq n' $ ts{ unique = n' }
  return n

-----------------------------------------------
-- TCMode

-- What are we checking
data TCMode
  = TCExpr          -- doing type checking
  | TCType          -- doing kind checking
  | TCKind          -- doing sort checking
  | TCSort          -- doing tier checking
  deriving (Show, Eq, Ord)

instance Enum TCMode where
  succ TCExpr = TCType
  succ TCType = TCKind
  succ TCKind = TCSort
  succ TCSort = error "succ TCSort"
  toEnum = undefined
  fromEnum = undefined

assertTCMode :: forall a . HasCallStack => (TCMode -> Bool) -> T a -> T a
--assertTCMode _ ta | usingMhs = ta
assertTCMode p ta = do
  tcm <- gets tcMode
  if p tcm then ta else error $ "assertTCMode: expected=" ++ show (filter p [TCExpr,TCType,TCKind]) ++ ", got=" ++ show tcm

-----------------------------------------------

type T a = TC TCState a

type Typed a = (a, EType)

getAppCon :: HasCallStack => EType -> Ident
getAppCon (EVar i) = i
getAppCon (ECon i) = conIdent i
getAppCon (EApp f _) = getAppCon f
getAppCon e = error $ "getAppCon: " ++ showExpr e

-----------------------------------------------

addConstraints :: [EConstraint] -> EType -> EType
addConstraints []  t = t
addConstraints cs  t = tupleConstraints cs `tImplies` t

tupleConstraints :: [EConstraint] -> EConstraint
tupleConstraints [c] = c
tupleConstraints cs  = tApps (tupleConstr noSLoc (length cs)) cs

-----------------------------------------------

tConI :: SLoc -> String -> EType
tConI loc = tCon . mkIdentSLoc loc

tCon :: Ident -> EType
tCon = EVar

tVarK :: IdKind -> EType
tVarK = EVar . idKindIdent

tApp :: EType -> EType -> EType
tApp = EApp

tApps :: Ident -> [EType] -> EType
tApps i ts = eApps (tCon i) ts

infixr `tArrow`
tArrow :: EType -> EType -> EType
tArrow a r = tApp (tApp (tConI builtinLoc nameArrow) a) r

tImplies :: EType -> EType -> EType
tImplies a r = tApp (tApp (tConI builtinLoc nameImplies) a) r

etImplies :: EType -> EType -> EType
etImplies (EVar i) t | i == tupleConstr noSLoc 0 = t
etImplies a t = tImplies a t

mkEqType :: SLoc -> EType -> EType -> EConstraint
mkEqType loc t1 t2 = eAppI2 (mkIdentSLoc loc nameTypeEq) t1 t2

mkCoercible :: SLoc -> EType -> EType -> EConstraint
mkCoercible loc t1 t2 = eAppI2 (mkIdentSLoc loc nameCoercible) t1 t2
