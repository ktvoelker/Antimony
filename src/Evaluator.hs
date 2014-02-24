
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

evalExpr :: Expr Unique -> EvalM (Expr Unique)
evalExpr = todo

exprResources :: Expr Unique -> [Resource]
exprResources = todo

