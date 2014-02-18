
module Sorter where

import H.Common
import qualified H.RefGraph as RG

import Monad
import Syntax

type SortM = ReaderT [Unique] (RG.RefGraphT Unique M)

sortPhase :: Namespace Unique -> M (Namespace Unique)
sortPhase =
  checked . liftM fst . RG.runRefGraphT . flip runReaderT [] . sortNamespace

sortNamespace :: Namespace Unique -> SortM (Namespace Unique)
sortNamespace = sortScopeMap $ onSndF sortDecl

sortScopeMap :: (a -> SortM a) -> [(Unique, a)] -> SortM [(Unique, a)]
sortScopeMap f bs = do
  let ids = map fst bs
  lift $ mapM_ RG.addDefM ids
  bs' <- mapM f' bs
  -- 1. Extract a subgraph of the RefGraph for just the nodes defined here
  wholeGraph <- get
  let curGraph = RG.filter (`elem` ids) wholeGraph
  -- 2. Get the SCC list
  let sccs = RG.sccs curGraph
  -- 3. Check the SCC list for cycles
  when (any RG.isCycle sccs) . lift2 . report
    $ Err (ECustom ECircRef) Nothing Nothing Nothing
  -- 4. Flatten the SCC list
  let sortedIds = RG.flattenSCCs sccs
  -- 5. Pair that list of Uniques with their sorted RHS values
  return $ map (\id -> (id, fromJust $ lookup id bs')) sortedIds
  where
    f' (id, rhs) = local (id :) $ (id,) <$> f rhs

sortDecl :: Decl Unique -> SortM (Decl Unique)
sortDecl (DNamespace ns) = DNamespace <$> sortNamespace ns
sortDecl (DType bs) = DType <$> sortScopeMap (onSndF sortBoundExpr) bs
sortDecl (DVal be) = DVal <$> sortBoundExpr be
sortDecl d@(DPrim _) = pure d

sortBoundExpr :: BoundExpr Unique -> SortM (BoundExpr Unique)
sortBoundExpr (BoundExpr ty val) =
  BoundExpr <$> sortType ty <*> maybe (pure Nothing) ((Just <$>) . sortExpr) val

sortType :: Type Unique -> SortM (Type Unique)
sortType (TFun ps b) = TFun <$> mapM sortType ps <*> sortType b
sortType (TRef qual) = TRef <$> sortQual qual

sortQual :: Qual Unique -> SortM (Qual Unique)
sortQual q@(Qual id _) = ask >>= lift . mapM_ (flip RG.addRefM id) >> pure q

sortExpr :: Expr Unique -> SortM (Expr Unique)
sortExpr (ERec bs) = ERec <$> sortScopeMap sortExpr bs
sortExpr (ERef qual) = ERef <$> sortQual qual
sortExpr (EFun ps body) = EFun ps <$> sortExpr body
sortExpr (EApp fn args) = EApp <$> sortExpr fn <*> mapM sortExpr args
sortExpr e@(ELit _) = pure e
sortExpr e@(EPrim _) = pure e

