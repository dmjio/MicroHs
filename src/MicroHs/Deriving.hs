module MicroHs.Deriving(deriveStrat, expandField, mkGetName) where
import Prelude(); import MHSPrelude
import Data.Char
import Data.Function
import Data.List
import MicroHs.Builtin
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.TCMonad
import Debug.Trace

-- Deriving runs when types level names are resolved, but not value level names.
-- To get access to names that might not be in scope, the module Mhs.Builtin
-- re-exports all names needed here.  This module is automagically imported as B@
-- Generated names should be like
--   type/class names fully qualified
--   method names (on lhs) unqualified
--   constructor names in the derived type unqualified
--   all other names should be qualified with B@

deriveStrat :: Maybe EConstraint -> Bool -> LHS -> [Constr] -> DerStrategy -> EConstraint -> T [EDef]
deriveStrat mctx newt lhs cs strat cls =
  case strat of
    DerNone | newt && useNewt cls -> newtypeDer  mctx lhs (cs!!0) cls Nothing
            | otherwise           -> deriveNoHdr mctx lhs cs cls
    DerStock                      -> deriveNoHdr mctx lhs cs cls
    DerNewtype | newt             -> newtypeDer  mctx lhs (cs!!0) cls Nothing
    DerAnyClass                   -> anyclassDer mctx lhs cls
    DerVia via | newt             -> newtypeDer  mctx lhs (cs!!0) cls (Just via)
    _                             -> cannotDerive lhs cls
  where useNewt d = unIdent (getAppCon d) `notElem`
          ["Data.Data.Data", "Data.Typeable.Typeable", "GHC.Generics.Generic",
           "Text.Read.Internal.Read", "Text.Show.Show"]

type DeriverT = LHS -> [Constr] -> EConstraint -> T [EDef]   -- Bool indicates a newtype
type Deriver = Maybe EConstraint -> DeriverT

derivers :: [(String, Deriver)]
derivers =
  [("Data.Bounded.Bounded",            derBounded)
  ,("Data.Enum.Enum",                  derEnum)
  ,("Data.Data.Data",                  derData)
  ,("Data.Eq.Eq",                      derEq)
  ,("Data.Ix.Ix",                      derNotYet)
  ,("Data.Ord.Ord",                    derOrd)
  ,("Data.Typeable.Typeable",          derTypeable)
  ,("GHC.Generics.Generic",            derNotYet)
  ,("Language.Haskell.TH.Syntax.Lift", derLift)
  ,("Text.Read.Internal.Read",         derRead)
  ,("Text.Show.Show",                  derShow)
  ]

deriveNoHdr :: Maybe EConstraint -> DeriverT
deriveNoHdr mctx lhs cs d = do
  case getDeriver d of
    Just f -> f mctx lhs cs d
    _      -> cannotDerive lhs d

getDeriver :: EConstraint -> Maybe Deriver
getDeriver d = lookup (unIdent $ getAppCon d) derivers

derNotYet :: Deriver
derNotYet _ _ _ d = do
  notYet d
  return []

notYet :: EConstraint -> T ()
notYet d =
  traceM ("Warning: cannot derive " ++ show d ++ " yet, " ++ showSLoc (getSLoc d))

-- We will never have Template Haskell, but we pretend we can derive Lift for it.
derLift :: Deriver
derLift _ _ _ _ = return []

--------------------------------------------

expandField :: EDef -> T [EDef]
expandField def@(Data    lhs cs _) = (++ [def]) <$> genHasFields lhs cs
expandField def@(Newtype lhs  c _) = (++ [def]) <$> genHasFields lhs [c]
expandField def                    = return [def]

genHasFields :: LHS -> [Constr] -> T [EDef]
genHasFields lhs cs = do
  let fldtys = nubBy ((==) `on` fst) [ (fld, ty) | Constr _ _ _ (Right fs) <- cs, (fld, (_, ty)) <- fs ]
--      flds = map fst fldtys
  concat <$> mapM (genHasField lhs cs) fldtys

genHasField :: LHS -> [Constr] -> (Ident, EType) -> T [EDef]
genHasField (tycon, iks) cs (fld, fldty) = do
  mn <- gets moduleName
  let loc = getSLoc tycon
      qtycon = qualIdent mn tycon
      eFld = EVar fld
      ufld = unIdent fld
      undef = mkExn loc ufld "recSelError"
      iHasField = mkIdentSLoc loc nameHasField
      iSetField = mkIdentSLoc loc nameSetField
      igetField = mkIdentSLoc loc namegetField
      isetField = mkIdentSLoc loc namesetField
      hdrGet = eForall iks $ eApp3 (EVar iHasField)
                                   (ELit loc (LStr ufld))
                                   (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                   fldty
      hdrSet = eForall iks $ eApp3 (EVar iSetField)
                                   (ELit loc (LStr ufld))
                                   (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                   fldty
      conEqnGet (Constr _ _ c (Left ts))   = eEqn [eApps (EVar c) (map (const eDummy) ts)] $ undef
      conEqnGet (Constr _ _ c (Right fts)) = eEqn [conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = eFld
      conEqnSet (Constr _ _ c (Left ts))   = eEqn [eDummy, eApps (EVar c) (map (const eDummy) ts)] $ undef
      conEqnSet (Constr _ _ c (Right fts)) = eEqn [eDummy, conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = eLam [eFld] conApp
      getName = mkGetName tycon fld

      -- XXX A hack, we can't handle forall yet.
      validType (EForall _ _ _) = False
      validType _ = True

  pure $ [ Sign [getName] $ eForall iks $ lhsToType (qtycon, iks) `tArrow` fldty
         , Fcn getName $ map conEqnGet cs ]
    ++ if not (validType fldty) then [] else
         [ instanceBody hdrGet [Fcn igetField [eEqn [eDummy] $ EVar getName] ]
         , instanceBody hdrSet [Fcn isetField $ map conEqnSet cs]
         ]

nameHasField :: String
nameHasField = "Data.Records.HasField"

nameSetField :: String
nameSetField = "Data.Records.SetField"

namegetField :: String
namegetField = "getField"

namesetField :: String
namesetField = "setField"

mkGetName :: Ident -> Ident -> Ident
mkGetName tycon fld = qualIdent (mkIdent "get") $ qualIdent tycon fld

--------------------------------------------

derTypeable :: Deriver
derTypeable _ (i, _) _ etyp = do
  mn <- gets moduleName
  let
    loc = getSLoc i
    itypeRep  = mkIdentSLoc loc "typeRep"
    imkTyConApp = mkBuiltin loc "mkTyConApp"
    imkTyCon = mkBuiltin loc "mkTyCon"
    hdr = EApp etyp (EVar $ qualIdent mn i)
    mdl = ELit loc $ LStr $ unIdent mn
    nam = ELit loc $ LStr $ unIdent i
    eqns = eEqns [eDummy] $ eAppI2 imkTyConApp (eAppI2 imkTyCon mdl nam) (EListish (LList []))
    inst = instanceBody hdr [Fcn itypeRep eqns]
  return [inst]

--------------------------------------------

getFieldTys :: (Either [SType] [ConstrField]) -> [EType]
getFieldTys (Left ts) = map snd ts
getFieldTys (Right ts) = map (snd . snd) ts

decomp :: EType -> [EType]
decomp t =
  case getAppM t of
    Just (c, ts) | isConIdent c -> concatMap decomp ts
    _                           -> [t]

-- If there is no mctx we use the default strategy to derive the instance context.
-- The default strategy basically extracts all subtypes with variables.
mkHdr :: Maybe EConstraint -> LHS -> [Constr] -> EConstraint -> T EConstraint
mkHdr (Just ctx) _ _ _ = return ctx
mkHdr _ lhs@(_, iks) cs cls = do
  ty <- mkLhsTy lhs
  let ctys :: [EType]  -- All top level types used by the constructors.
      ctys = nubBy eqEType [ tt | Constr evs _ _ flds <- cs, ft <- getFieldTys flds, tt <- decomp ft,
                            not $ null $ freeTyVars [tt] \\ map idKindIdent evs, not (eqEType ty tt) ]
  pure $ eForall iks $ addConstraints (map (tApp cls) ctys) $ tApp cls ty

mkLhsTy :: LHS -> T EType
mkLhsTy (t, iks) = do
  mn <- gets moduleName
  return $ tApps (qualIdent mn t) $ map tVarK iks

mkPat :: Constr -> String -> (EPat, [Expr])
mkPat (Constr _ _ c flds) s =
  let n = either length length flds
      loc = getSLoc c
      vs = map (EVar . mkIdentSLoc loc . (s ++) . show) [1..n]
  in  (tApps c vs, vs)

cannotDerive :: LHS -> EConstraint -> T [EDef]
cannotDerive (c, _) e = tcError (getSLoc e) $ "Cannot derive " ++ showEType (EApp e (EVar c))

--------------------------------------------

derEq :: Deriver
derEq mctx lhs cs@(_:_) eeq = do
  hdr <- mkHdr mctx lhs cs eeq
  let loc = getSLoc eeq
      mkEqn c =
        let (xp, xs) = mkPat c "x"
            (yp, ys) = mkPat c "y"
        in  eEqn [xp, yp] $ if null xs then eTrue else foldr1 eAnd $ zipWith eEq xs ys
      eqns = map mkEqn cs ++ [eEqn [eDummy, eDummy] eFalse]
      iEq = mkIdentSLoc loc "=="
      eEq = EApp . EApp (EVar $ mkBuiltin loc "==")
      eAnd = EApp . EApp (EVar $ mkBuiltin loc "&&")
      eTrue = EVar $ mkBuiltin loc "True"
      eFalse = EVar $ mkBuiltin loc "False"
      inst = instanceBody hdr [Fcn iEq eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derEq _ lhs _ e = cannotDerive lhs e

--------------------------------------------

derOrd :: Deriver
derOrd mctx lhs cs@(_:_) eord = do
  hdr <- mkHdr mctx lhs cs eord
  let loc = getSLoc eord
      mkEqn c =
        let (xp, xs) = mkPat c "x"
            (yp, ys) = mkPat c "y"
        in  [eEqn [xp, yp] $ if null xs then eEQ else foldr1 eComb $ zipWith eCompare xs ys
            ,eEqn [xp, eDummy] $ eLT
            ,eEqn [eDummy, yp] $ eGT]
      eqns = concatMap mkEqn cs
      iCompare = mkIdentSLoc loc "compare"
      eCompare = EApp . EApp (EVar $ mkBuiltin loc "compare")
      eComb = EApp . EApp (EVar $ mkBuiltin loc "<>")
      eEQ = EVar $ mkBuiltin loc "EQ"
      eLT = EVar $ mkBuiltin loc "LT"
      eGT = EVar $ mkBuiltin loc "GT"
      inst = instanceBody hdr [Fcn iCompare eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derOrd _ lhs _ e = cannotDerive lhs e

--------------------------------------------

derBounded :: Deriver
derBounded mctx lhs cs@(c0:_) ebnd = do
  hdr <- mkHdr mctx lhs cs ebnd
  let loc = getSLoc ebnd
      mkEqn bnd (Constr _ _ c flds) =
        let n = either length length flds
        in  eEqn [] $ tApps c (replicate n (EVar bnd))

      iMinBound = mkIdentSLoc loc "minBound"
      iMaxBound = mkIdentSLoc loc "maxBound"
      minEqn = mkEqn iMinBound c0
      maxEqn = mkEqn iMaxBound (last cs)
      inst = instanceBody hdr [Fcn iMinBound [minEqn], Fcn iMaxBound [maxEqn]]
  -- traceM $ showEDefs [inst]
  return [inst]
derBounded _ lhs _ e = cannotDerive lhs e

--------------------------------------------

derEnum :: Deriver
derEnum mctx lhs cs@(c0:_) enm | all isNullary cs = do
  hdr <- mkHdr mctx lhs cs enm
  let loc = getSLoc enm

      mkFrom (Constr _ _ c _) i =
        eEqn [EVar c] $ ELit loc (LInt i)
      mkTo (Constr _ _ c _) i =
        eEqn [ELit loc (LInt i)] $ EVar c
      eFirstCon = let Constr _ _ c _ = c0 in tCon c
      eLastCon = let Constr _ _ c _ = last cs in tCon c

      iFromEnum = mkIdentSLoc loc "fromEnum"
      iToEnum = mkIdentSLoc loc "toEnum"
      iEnumFrom = mkIdentSLoc loc "enumFrom"
      iEnumFromThen = mkIdentSLoc loc "enumFromThen"
      iEnumFromTo = mkBuiltin loc "enumFromTo"
      iEnumFromThenTo = mkBuiltin loc "enumFromThenTo"
      fromEqns = zipWith mkFrom cs [0..]
      toEqns   = zipWith mkTo   cs [0..] ++ [eEqn [eDummy] $ EApp (EVar $ mkBuiltin loc "error") (ELit loc (LStr "toEnum: out of range"))]
      enumFromEqn =
        -- enumFrom x = enumFromTo x (last cs)
        let x = EVar (mkIdentSLoc loc "x")
        in eEqn [x] (eAppI2 iEnumFromTo x eLastCon)
      enumFromThenEqn =
        -- enumFromThen x1 x2 = if fromEnum x2 >= fromEnum x1 then enumFromThenTo x1 x2 (last cs) else enumFromThenTo x1 x2 (head cs)
        let
          x1 = EVar (mkIdentSLoc loc "x1")
          x2 = EVar (mkIdentSLoc loc "x2")
        in eEqn [x1, x2] (EIf (eAppI2 (mkBuiltin loc ">=") (EApp (EVar iFromEnum) x2) (EApp (EVar iFromEnum) x1)) (eAppI3 iEnumFromThenTo x1 x2 eLastCon) (eAppI3 iEnumFromThenTo x1 x2 eFirstCon))
      inst = instanceBody hdr [Fcn iFromEnum fromEqns, Fcn iToEnum toEqns, Fcn iEnumFrom [enumFromEqn], Fcn iEnumFromThen [enumFromThenEqn]]
  --traceM $ showEDefs [inst]
  return [inst]
derEnum _ lhs _ e = cannotDerive lhs e

isNullary :: Constr -> Bool
isNullary (Constr _ _ _ flds) = either null null flds

--------------------------------------------

derShow :: Deriver
derShow mctx lhs cs@(_:_) eshow = do
  hdr <- mkHdr mctx lhs cs eshow
  let loc = getSLoc eshow
      mkEqn c@(Constr _ _ nm flds) =
        let (xp, xs) = mkPat c "x"
        in  eEqn [varp, xp] $ showRHS nm xs flds

      var = EVar . mkBuiltin loc
      varp = EVar $ mkIdent "p"
      lit = ELit loc

      iShowsPrec = mkIdentSLoc loc "showsPrec"
      eShowsPrec n = eApp2 (var "showsPrec") (lit (LInt n))
      eShowString s = EApp (var "showString") (lit (LStr s))
      eParen n = eApp2 (var "showParen") (eApp2 (var ">") varp (lit (LInt n)))
      eShowL s = foldr1 ejoin . intersperse (eShowString s)
      ejoin = eApp2 (var ".")

      showRHS nm [] _ = eShowString (unIdentPar nm)
      showRHS nm xs (Left   _) = showRHSN nm xs
      showRHS nm xs (Right fs) = showRHSR nm $ zip (map fst fs) xs

      showRHSN nm xs = eParen 10 $ eShowL " " $ eShowString (unIdentPar nm) : map (eShowsPrec 11) xs

      showRHSR nm fxs =
        eShowString (unIdentPar nm ++ "{") `ejoin`
        (eShowL "," $ map fld fxs) `ejoin`
        eShowString "}"
          where fld (f, x) = eShowString (unIdentPar f ++ "=") `ejoin` eShowsPrec 0 x

      eqns = map mkEqn cs
      inst = instanceBody hdr [Fcn iShowsPrec eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derShow _ lhs _ e = cannotDerive lhs e

unIdentPar :: Ident -> String
unIdentPar i =
  let s = unIdent i
  in  if isAlpha (head s) then s else "(" ++ s ++ ")"

--------------------------------------------

-- Deriving for the fake Data class.
derData :: Deriver
derData mctx lhs cs edata = do
  notYet edata
  hdr <- mkHdr mctx lhs cs edata
  let
    inst = instanceBody hdr []
  return [inst]

--------------------------------------------

derRead :: Deriver
derRead mctx lhs cs eread = do
  notYet eread
  hdr <- mkHdr mctx lhs cs eread
  let
    loc = getSLoc eread
    iReadPrec = mkIdentSLoc loc "readPrec"
    err = eEqn [] $ EApp (EVar $ mkBuiltin loc "error") (ELit loc (LStr "readPrec not defined"))
    inst = instanceBody hdr [Fcn iReadPrec [err]]
  return [inst]

--------------------------------------------

newtypeDer :: Maybe EConstraint -> LHS -> Constr -> EConstraint -> Maybe EConstraint -> T [EDef]
newtypeDer mctx lhs c cls mvia = do
  hdr <- mkHdr mctx lhs [c] cls
  let cty =
        case c of
          Constr [] [] _ (Left [(False, t)]) -> t
          Constr [] [] _ (Right [(_, (_, t))]) -> t
          _ -> error "newtypeDer"
      mvia' = fmap (tApp cls) mvia
--  traceM ("newtypeDer: " ++ show hdr)
  return [Instance hdr $ InstanceVia (tApp cls cty) mvia']

anyclassDer :: Maybe EConstraint -> LHS -> EConstraint -> T [EDef]
anyclassDer mctx lhs cls = do
  hdr <- mkHdr mctx lhs [] cls
  return [instanceBody hdr []]
