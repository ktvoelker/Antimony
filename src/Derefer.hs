
module Derefer where

import H.Common

import Monad
import Syntax

type Env = [(Unique, [(Text, Unique)])]

makeEnv :: DeclMapElem Unique Decl -> Env
makeEnv = uncurry makeEnvDecl . onSnd snd

makeEnvDecl :: Unique -> Decl Unique -> Env
makeEnvDecl u (DNamespace ds) = return (u, map makeEnvElem ds) <> (ds >>= makeEnv)
makeEnvDecl _ (DType _) = mempty
makeEnvDecl _ (DVal _) = mempty
makeEnvDecl _ (DPrim _) = mempty

makeEnvElem :: DeclMapElem Unique Decl -> (Text, Unique)
makeEnvElem (u, _) = (uniqueSourceName u, u)

type DerM = ReaderT Env M

runDerM :: Namespace Unique -> DerM a -> M a
runDerM = flip runReaderT . (>>= makeEnv)

derefPhase :: Namespace Unique -> M (Namespace Unique)
derefPhase ns = checked . runDerM ns $ derefNamespace ns

derefNamespace :: Namespace Unique -> DerM (Namespace Unique)
derefNamespace = derefDeclMap derefDecl

derefDeclMap
  :: (d Unique -> DerM (d Unique))
  -> DeclMap Unique d
  -> DerM (DeclMap Unique d)
derefDeclMap = mapM . onSndF . onSndF

derefDecl :: Decl Unique -> DerM (Decl Unique)
derefDecl (DNamespace ds) = DNamespace <$> derefNamespace ds
derefDecl (DType ds) = DType <$> derefDeclMap derefBoundExpr ds
derefDecl (DVal be) = DVal <$> derefBoundExpr be
derefDecl d@(DPrim _) = pure d

derefBoundExpr :: BoundExpr Unique -> DerM (BoundExpr Unique)
derefBoundExpr (BoundExpr ty rhs) =
  BoundExpr <$> derefType ty <*> maybe (pure Nothing) ((Just <$>) . derefExpr) rhs

derefType :: Type Unique -> DerM (Type Unique)
derefType (TFun ps r) = TFun <$> mapM derefType ps <*> derefType r
derefType (TRef qual) = TRef <$> derefQual qual

derefExpr :: Expr Unique -> DerM (Expr Unique)
derefExpr e@(ELit _) = pure e
derefExpr (EFun ps e) = EFun ps <$> derefExpr e
derefExpr (ERef qual) = ERef <$> derefQual qual
derefExpr (EApp fn as) = EApp <$> derefExpr fn <*> mapM derefExpr as
derefExpr (ERec ty bs) = ERec ty <$> mapM (onSndF derefExpr) bs
derefExpr e@(EPrim _) = pure e

derefQual :: Qual Unique -> DerM (Qual Unique)
derefQual (Qual u fs) = go u fs
  where
    go u [] = return $ Qual u []
    go u (f : fs) = asks (lookup u) >>= \case
      Nothing -> fatal $ Err (ECustom EDeref) Nothing Nothing Nothing
      Just ds -> case lookup f ds of
        Nothing -> fatal $ Err (ECustom EUnbound) Nothing Nothing Nothing
        Just u' -> go u' fs

