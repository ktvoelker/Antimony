
module Renamer (renamePhase) where

import qualified Data.Map as M
import H.Common
import H.Monad.Scope

import Monad
import Syntax

type RenM = ScopeT Id Unique M

renamePhase :: FileMap (Namespace Id) -> M (Namespace Unique)
renamePhase = stage ARename . runScopeT . renameFiles

renameFiles :: FileMap (Namespace Id) -> RenM (Namespace Unique)
renameFiles = mergeNamespaces . M.elems >=> renameNamespace

mergeNamespaces :: [Namespace Id] -> RenM (Namespace Id)
mergeNamespaces = foldM (unionWithM merge) M.empty

merge :: Decl Id -> Decl Id -> RenM (Decl Id)
merge (DNamespace ax dx) (DNamespace ay dy)
  | ax == ay  = DNamespace ax <$> mergeNamespaces [dx, dy]
  | otherwise = fatal $ Err (ECustom EMergeAccess) Nothing Nothing Nothing
merge _ _ = fatal $ Err (ECustom EMerge) Nothing Nothing Nothing

renameNamespace :: Namespace Id -> RenM (Namespace Unique)
renameNamespace bs =
  scope' (nextUnique . idText) (M.keys bs)
  . (M.fromList <$>)
  . mapM (\(k, v) -> (,) <$> findInScope k <*> renameDecl v)
  . M.toList
  $ bs

renameExpr :: Expr Id -> RenM (Expr Unique)
-- TODO
renameExpr = undefined

renameDecl :: Decl Id -> RenM (Decl Unique)
renameDecl (DVal (BoundExpr a t e)) =
  (DVal .) . BoundExpr a
    <$> renameType t
    <*> maybe (pure Nothing) ((Just <$>) . renameExpr) e
-- TODO
renameDecl (DType bs) = undefined bs
renameDecl (DNamespace acc bs) = undefined acc bs

renameType :: Type Id -> RenM (Type Unique)
-- TODO
renameType = undefined

