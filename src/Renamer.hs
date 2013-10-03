
module Renamer (renamePhase) where

import qualified Data.Map as M
import H.Common
import H.Monad.Scope

import Monad
import Syntax

type RenM = ScopeT Id Unique M

renamePhase :: RecBody Id -> M (RecBody Unique)
renamePhase = stage ARename . runScopeT . renameRecBody

renameRecBody :: RecBody Id -> RenM (RecBody Unique)
renameRecBody bs =
  scope' (nextUnique . idText) (M.keys bs)
  . (M.fromList <$>)
  . mapM (\(k, v) -> (,) <$> findInScope k <*> renameATE v)
  . M.toList
  $ bs
  where
    renameATE (a, (t, e)) = (a,) . (t,) <$> renameExpr e

renameExpr :: Expr Id -> RenM (Expr Unique)
renameExpr = undefined

