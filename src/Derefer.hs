
module Derefer where

import H.Common

import Monad
import Syntax

data EnvElem = EnvNamespace Unique | EnvType Unique

type Env = Map Unique (Map Text EnvElem)

makeEnv :: Namespace Unique -> Env
makeEnv = undefined

type DerM = ReaderT Env M

runDerM :: Namespace Unique -> DerM a -> M a
runDerM = flip runReaderT . makeEnv

derefPhase :: Namespace Unique -> M (Namespace Unique)
derefPhase ns = stage ADeref . runDerM ns $ derefNamespace ns

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
derefExpr (ERec bs) = ERec <$> mapM (onSndF derefExpr) bs
derefExpr (EGet _ _) = impossible "EGet in derefExpr"
derefExpr e@(EPrim _) = pure e

derefQual :: Qual Unique -> DerM (Qual Unique)
derefQual = undefined

