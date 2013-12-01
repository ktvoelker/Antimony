
module Prim where

import H.Common

import Syntax

primConcat = primId "concat"

primOps :: [(PrimId, Type PrimId)]
primOps = [(primConcat, undefined)]

liftPrimType :: Type PrimId -> Type Id
liftPrimType (TFun ts t) = TFun (map liftPrimType ts) $ liftPrimType t
liftPrimType (TRef (Qual id ms)) = TRef (Qual (Prim id) ms)

addPrimOps :: Namespace Id -> Namespace Id
addPrimOps = (map (\(id, ty) -> (Prim id, f id ty)) primOps ++)
  where
    f id ty = (Public, DVal $ BoundExpr (liftPrimType ty) (Just $ EPrim id))

