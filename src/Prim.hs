
module Prim where

import H.Common

import Syntax

primTypeRef :: PrimId -> Type PrimId
primTypeRef = TRef . flip Qual []

primFun :: [PrimId] -> PrimId -> Type PrimId
primFun as = TFun (map primTypeRef as) . primTypeRef

primConcat = primId "concat"

primStr = primId "str"

primInt = primId "int"

primBool = primId "bool"

primOps :: [(PrimId, Type PrimId)]
primOps = [(primConcat, primFun [primStr, primStr] primStr)]

primLiterals :: [PrimId]
primLiterals = [primStr, primInt, primBool]

primResources :: [(PrimId, [(Text, PrimId)])]
primResources = []

liftPrimType :: Type PrimId -> Type Id
liftPrimType (TFun ts t) = TFun (map liftPrimType ts) $ liftPrimType t
liftPrimType (TRef (Qual id ms)) = TRef (Qual (Prim id) ms)

liftPrimTypeUnique :: Type PrimId -> Type Unique
liftPrimTypeUnique (TFun ts t) = TFun (map liftPrimTypeUnique ts) $ liftPrimTypeUnique t
liftPrimTypeUnique (TRef (Qual id ms)) = TRef (Qual (primUnique id) ms)

addPrimitives :: Namespace Id -> Namespace Id
addPrimitives = (primOps' ++) . (primLiterals' ++) . (primResources' ++)
  where
    -- TODO there's a lot of redundancy here
    primOps' = map (\(id, ty) -> (Prim id, f id ty)) primOps
    f id ty = (Public, DVal $ BoundExpr (liftPrimType ty) (Just $ EPrim id))
    primLiterals' = map (\id -> (Prim id, (Public, DPrim id))) primLiterals
    primResources' =
      map (\(id, as) -> (Prim id, (Public, DType . map g $ as))) primResources
    g (name, ty) =
      (Id name, (Public, BoundExpr (liftPrimType $ primTypeRef ty) Nothing))

litMatchesPrim :: Lit -> Unique -> Bool
litMatchesPrim (LitStr _)  = (== primUnique primStr)
litMatchesPrim (LitInt _)  = (== primUnique primInt)
litMatchesPrim (LitBool _) = (== primUnique primBool)

