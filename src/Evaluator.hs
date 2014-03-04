
module Evaluator where

import H.Common

import Monad
import Prim.Eval
import Resource
import Syntax
import Value

data Env = Env
  { envBindings :: [(Unique, Val)]
  , envRecords  :: [(Ptr, [(Text, Val)])]
  , envNextPtr  :: Ptr
  } deriving (Eq, Ord, Show)

type EvalM = StateT Env M

instance MonadHeap EvalM where
  deref ptr = lookup ptr <$> gets envRecords
  store rec = do
    ptr <- nextPtr
    modify $ \s -> s { envRecords = (ptr, rec) : envRecords s }
    return ptr

nextPtr :: EvalM Ptr
nextPtr = do
  ptr@(Ptr n) <- gets envNextPtr
  modify $ \s -> s { envNextPtr = Ptr (n + 1) }
  return ptr

evalPhase :: Namespace Unique -> M [Resource]
evalPhase ns = evalStateT (evaluate ns) $ Env [] [] (Ptr 1)

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
      v <- evalExpr expr
      modify $ \s -> s { envBindings = (u, v) : envBindings s }
      valueResources v

getVal :: Unique -> EvalM Val
getVal u =
  gets envBindings
  >>= maybe (impossible "Name not found in getVal") return . lookup u

localBindings :: [(Unique, Val)] -> EvalM a -> EvalM a
localBindings inner m = do
  outer <- gets envBindings
  modify $ \s -> s { envBindings = inner ++ outer }
  ret <- m
  modify $ \s -> s { envBindings = outer }
  return ret

evalExpr :: Expr Unique -> EvalM Val
evalExpr (ELit lit) = return $ VLit lit
evalExpr (EFun ps body) = return $ VFun ps body
evalExpr (EPrim id) = return $ VPrim id
evalExpr (ERef (Qual u fs)) = getVal u >>= flip (foldM lookupField) fs
evalExpr (EApp fn args) =
  evalExpr fn >>= \case
    VFun ps body -> do
      args' <- zip ps <$> mapM evalExpr args
      localBindings args' $ evalExpr body
    VPrim id -> evalPrim id <$> mapM evalExpr args
    _ -> impossible "Application of a non-function"
evalExpr (ERec fs) = VRec <$> (f (unzip fs) >>= store)
  where
    f (names, values) = zip (map uniqueSourceName names) <$> mapM evalExpr values

lookupField :: Val -> Text -> EvalM Val
lookupField (VRec ptr) f = do
  ((lookup ptr >=> lookup f) <$> gets envRecords) >>= \case
    Nothing -> impossible "Ptr or field lookup failed"
    Just val -> return val
lookupField _ _ = impossible "Field lookup from a non-record"

valueResources :: Val -> EvalM [Resource]
valueResources = todo

