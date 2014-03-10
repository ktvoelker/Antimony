
module Evaluator where

import H.Common

import Monad
import Prim.Eval
import Syntax
import Value

data Env = Env
  { envBindings  :: [(Unique, Val)]
  , envNamespace :: [Unique]
  , envRecords   :: Heap
  , envMain      :: Maybe Ptr
  , envNextPtr   :: Ptr
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

evalPhase :: Namespace Unique -> M (Ptr, Heap)
evalPhase ns = evalStateT (evaluate ns) $ Env [] [] [] Nothing (Ptr 1)

evaluate :: Namespace Unique -> EvalM (Ptr, Heap)
evaluate ns = do
  e ns
  (,) <$> (maybe (error "missing main.main") id <$> gets envMain) <*> gets envRecords
  where
    -- Evaluate an entire namespace
    e ns = mapM_ f ns
    -- Evaluate one binding in a namespace
    f (u, (_, d)) = case d of
      DNamespace ds -> localNamespace u $ e ds
      DType fs -> mapM_ g fs
      DVal be -> h u be
      DPrim _ -> return ()
    -- Evaluate one binding in a type declaration
    g (u, (_, be)) = h u be
    -- Evaluate a bound expression
    h _ (BoundExpr _ Nothing) = return ()
    h u (BoundExpr _ (Just expr)) = do
      ns <- gets envNamespace
      v <- evalExpr expr
      let inMainModule = map uniqueSourceName (take 1 ns) == ["main"]
      let inMainFunction = uniqueSourceName u == "main"
      when (inMainModule && inMainFunction) $
        case v of
          VRec ptr -> modify $ \s -> s { envMain = Just ptr }
          _ -> error "main.main is not a record"
      modify $ \s -> s { envBindings = (u, v) : envBindings s }

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

localNamespace :: Unique -> EvalM a -> EvalM a
localNamespace u m = do
  modify $ \s -> s { envNamespace = u : envNamespace s }
  ret <- m
  modify $ \s -> s { envNamespace = tail $ envNamespace s }
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
evalExpr (ERec ty fs) = VRec <$> (f (unzip fs) >>= store . Rec ty)
  where
    f (names, values) = zip (map uniqueSourceName names) <$> mapM evalExpr values

lookupField :: Val -> Text -> EvalM Val
lookupField (VRec ptr) f = do
  ((lookup ptr >=> lookup f . recBody) <$> gets envRecords) >>= \case
    Nothing -> impossible "Ptr or field lookup failed"
    Just val -> return val
lookupField _ _ = impossible "Field lookup from a non-record"

