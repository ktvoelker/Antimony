
module Renamer (renamePhase) where

import qualified Data.Map as M
import H.Common
import H.Scope

import Monad
import Prim
import Syntax

type RenM = ScopeT Id Unique M

renamePhase :: FileMap (Namespace Id) -> M (Namespace Unique)
renamePhase = checked . runScopeT . renameFiles

renameFiles :: FileMap (Namespace Id) -> RenM (Namespace Unique)
renameFiles = M.elems >>> mergeNamespaces >=> addPrimitives >>> renameNamespace

mergeNamespaces :: [Namespace Id] -> RenM (Namespace Id)
mergeNamespaces = liftM M.toList . foldM (unionWithM merge) M.empty . map M.fromList

merge :: (Access, Decl Id) -> (Access, Decl Id) -> RenM (Access, Decl Id)
merge (ax, DNamespace dx) (ay, DNamespace dy)
  | ax == ay  = (ax,) . DNamespace <$> mergeNamespaces [dx, dy]
  | otherwise = fatal $ Err (ECustom EMergeAccess) Nothing Nothing Nothing
merge _ _ = fatal $ Err (ECustom EMerge) Nothing Nothing Nothing

scopeForBindings :: [Id] -> RenM a -> RenM a
scopeForBindings = scope' f
  where
    f (Id xs) = nextUnique xs
    f (Prim id) = pure $ primUnique id

renameQual :: Qual Id -> RenM (Qual Unique)
renameQual (Qual id ms) = Qual <$> findInScope id <*> pure ms

renameNamespace :: Namespace Id -> RenM (Namespace Unique)
renameNamespace = renameScopeMap $ onSndF renameDecl

renameScopeMap :: (a -> RenM b) -> [(Id, a)] -> RenM [(Unique, b)]
renameScopeMap f bs = scopeForBindings (map fst bs) $ mapM f' bs
  where
    f' (id, val) = (,) <$> findInScope id <*> f val

renameExpr :: Expr Id -> RenM (Expr Unique)
renameExpr (EFun ps b) =
  scopeForBindings ps $ EFun <$> mapM findInScope ps <*> renameExpr b
renameExpr (ERec ty bs) = ERec ty <$> renameScopeMap renameExpr bs
renameExpr (ERef qual) = ERef <$> renameQual qual
renameExpr (EApp fn args) = EApp <$> renameExpr fn <*> mapM renameExpr args
renameExpr (ELit lit) = pure $ ELit lit
renameExpr (EPrim id) = pure $ EPrim id

renameDecl :: Decl Id -> RenM (Decl Unique)
renameDecl (DVal b) = DVal <$> renameBoundExpr b
renameDecl (DType bs) = DType <$> renameScopeMap (onSndF renameBoundExpr) bs
renameDecl (DNamespace ds) = DNamespace <$> renameNamespace ds
renameDecl (DPrim id) = pure $ DPrim id

renameType :: Type Id -> RenM (Type Unique)
renameType (TFun ps r) = TFun <$> mapM renameType ps <*> renameType r
renameType (TRef qual) = TRef <$> renameQual qual

renameBoundExpr :: BoundExpr Id -> RenM (BoundExpr Unique)
renameBoundExpr (BoundExpr ty body) =
  BoundExpr <$> renameType ty <*> maybe (pure Nothing) ((Just <$>) . renameExpr) body

