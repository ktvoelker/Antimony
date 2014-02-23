
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
evaluate = todo

