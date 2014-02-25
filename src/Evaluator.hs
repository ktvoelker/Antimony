
module Evaluator where

import H.Common

import Monad
import Resource
import Syntax

data EnvElem =
    EnvType [(Text, Maybe (Expr Unique))]
  | EnvValue (Maybe (Expr Unique))
  deriving (Show)

type Env = [(Unique, EnvElem)]

type EvalM = StateT Env M

makeEnv :: Namespace Unique -> Env
makeEnv = (>>= f)
  where
    f (u, (_, d)) = case d of
      DNamespace ds -> makeEnv ds
      DType fs -> return (u, EnvType $ map g fs)
      DVal (BoundExpr _ expr) -> return (u, EnvValue expr)
      DPrim _ -> mempty
    g (u, (_, BoundExpr _ expr)) = (uniqueSourceName u, expr)

evalPhase :: Namespace Unique -> M [Resource]
evalPhase ns = evalStateT (evaluate ns) $ makeEnv ns

evaluate :: Namespace Unique -> EvalM [Resource]
evaluate ns = concat <$> mapM f ns
  where
    f (u, (_, d)) = case d of
      DNamespace ds -> evaluate ds
      DType fs -> concat <$> mapM g fs
      DVal be -> h u be
      DPrim _ -> return mempty
    g (u, (_, be)) = h u be
    h _ (BoundExpr _ Nothing) = return mempty
    h u (BoundExpr _ (Just expr)) = do
      expr' <- evalExpr expr
      modify $ ((u, EnvValue . Just $ expr') :)
      return $ exprResources expr'

getValue :: Unique -> EvalM (Expr Unique)
getValue u = get >>= maybe (impossible "Name not found in getValue") f . lookup u
  where
    f (EnvType _) = impossible "Type found in getValue"
    f (EnvValue Nothing) = impossible "Abstract value found in getValue"
    f (EnvValue (Just e)) = return e

evalExpr :: Expr Unique -> EvalM (Expr Unique)
evalExpr e@(ELit _) = return e
evalExpr e@(EFun _ _) = return e
evalExpr (ERef (Qual u fs)) = getValue u >>= flip (foldM lookupField) fs
evalExpr (EApp fn args) =
  evalExpr fn >>= \case
    EFun ps body -> mapM evalExpr args >>= evalExpr . subst body . zip ps
    EPrim _ -> todo
    _ -> impossible "Application of a non-function"
evalExpr (ERec fs) = ERec <$> f (unzip fs)
  where
    f (names, values) = zip names <$> mapM evalExpr values
evalExpr e@(EPrim _) = return e

subst :: Expr Unique -> [(Unique, Expr Unique)] -> Expr Unique
subst = todo

lookupField :: Expr Unique -> Text -> EvalM (Expr Unique)
lookupField (ERec fs) f =
  maybe todo return . lookup f . map (onFst uniqueSourceName) $ fs
lookupField _ _ = impossible "Field lookup from a non-record"

exprResources :: Expr Unique -> [Resource]
exprResources = todo

