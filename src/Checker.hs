
module Checker where

import H.Common

import Monad
import Prim
import Syntax

data EnvElem = EnvType [(Text, Type Unique)] | EnvValue (Type Unique) deriving (Show)

type Env = [(Unique, EnvElem)]

type ChkM = ReaderT Env M

makeEnv :: Namespace Unique -> Env
makeEnv = (>>= f)
  where
    f (u, (_, d)) = case d of
      DNamespace ds -> makeEnv ds
      DType fs -> (u, EnvType $ map g fs) : (fs >>= h . boundExprValue . snd . snd)
      DVal (BoundExpr ty expr) -> (u, EnvValue ty) : h expr
      DPrim _ -> mempty
    g (u, (_, BoundExpr ty _)) = (uniqueSourceName u, ty)
    h :: Maybe (Expr Unique) -> Env
    h = maybe mempty $ \case
      ELit _       -> mempty
      EFun ps e    -> map (\p -> (p, EnvValue todo)) ps `mplus` h (Just e)
      ERef _       -> mempty
      EApp fn args -> h (Just fn) `mplus` (args >>= h . Just)
      ERec bs      -> bs >>= h . Just . snd
      EPrim _      -> mempty

checkPhase :: Namespace Unique -> M (Namespace Unique)
checkPhase b = stage ACheck . flip runReaderT (makeEnv b) . checkNamespace $ b

checkNamespace :: Namespace Unique -> ChkM (Namespace Unique)
checkNamespace = mapM . onSndF . onSndF $ checkDecl

checkDecl :: Decl Unique -> ChkM (Decl Unique)
checkDecl (DNamespace ds) = DNamespace <$> checkNamespace ds
checkDecl (DType fs) = DType <$> (mapM . onSndF . onSndF $ checkBoundExpr) fs
checkDecl (DVal be) = DVal <$> checkBoundExpr be
checkDecl d@(DPrim _) = return d

checkBoundExpr :: BoundExpr Unique -> ChkM (BoundExpr Unique)
checkBoundExpr be@(BoundExpr _ Nothing) = return be
checkBoundExpr be@(BoundExpr ty (Just e)) = match e ty >> return be

err :: AErrType -> ChkM ()
err e = report $ Err (ECustom e) Nothing Nothing Nothing

equate :: (Eq a) => a -> a -> ChkM ()
equate a b = when (a /= b) $ err EType

match :: Expr Unique -> Type Unique -> ChkM ()
match (ELit lit) (TRef (Qual u []))
  | litMatchesPrim lit u = return ()
  | otherwise = err EType
match (ELit _) _ = err EKind
match (EFun ps body) (TFun pTys bodyTy) =
  equate (length ps) (length pTys)
  >> match body bodyTy
match (EFun _ _) (TRef _) = err EKind
match (ERef (Qual u [])) ty = asks (lookup u) >>= \case
  Nothing -> err EKind
  Just (EnvType _) -> err EKind
  Just (EnvValue refTy) -> equate refTy ty
match (ERef _) _ = err EKind
match (EApp (ERef (Qual u [])) args) ty = asks (lookup u) >>= \case
  Nothing -> err EKind
  Just (EnvType _) -> err EKind
  Just (EnvValue (TFun pTys bodyTy)) ->
    equate (length args) (length pTys)
    >> mapM (uncurry match) (zip args pTys)
    >> equate bodyTy ty
  Just (EnvValue _) -> err EType
match (EApp (ERef _) _) _ = err EKind
match (EApp _ _) _ = impossible "Application of a non-reference"
match (ERec fs) (TRef (Qual u [])) = asks (lookup u) >>= \case
  Nothing -> err EKind
  Just (EnvValue _) -> err EKind
  Just (EnvType ts) -> matchRec fs ts
match (ERec _) _ = err EKind
match (EPrim id) ty = case lookup id primOps of
  Nothing -> impossible "Invalid primitive identifier"
  Just ty' -> equate (liftPrimTypeUnique ty') ty

matchRec :: [(Unique, Expr Unique)] -> [(Text, Type Unique)] -> ChkM ()
matchRec fs ts = equate fNames tNames >> mapM_ (uncurry match) (zip fExprs tTypes)
  where
    s = unzip . sortBy (compare `on` fst)
    (fNames, fExprs) = s . map (onFst uniqueSourceName) $ fs
    (tNames, tTypes) = s ts

