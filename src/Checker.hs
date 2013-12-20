
module Checker where

import H.Common

import Monad
import Prim
import Syntax

data EnvElem = EnvType [(Text, Type Unique)] | EnvValue (Type Unique) deriving (Show)

type Env = [(Unique, EnvElem)]

type ChkM = ReaderT Env M

-- TODO add EnvValue entries for all function parameters
makeEnv :: Namespace Unique -> Env
makeEnv = (>>= f)
  where
    f (u, (_, d)) = case d of
      DNamespace ds -> makeEnv ds
      DType fs -> return (u, EnvType $ map g fs)
      DVal (BoundExpr ty _) -> return (u, EnvValue ty)
      DPrim _ -> mempty
    g (u, (_, BoundExpr ty _)) = (uniqueSourceName u, ty)

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
match (ERef (Qual u [])) ty = asks (lookup u) >>= \case
  Nothing -> err EKind
  Just (EnvType _) -> err EKind
  Just (EnvValue refTy) -> equate refTy ty
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

