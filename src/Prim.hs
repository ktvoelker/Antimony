
module Prim
  ( addPrimitives
  , primOps
  , primConcat
  , primOr
  , liftPrimTypeUnique
  , primRef
  , litMatchesPrim
  , primStr
  , primInt
  , primBool
  , evalPrim
  ) where

import H.Common

import Syntax

primTypeRef :: PrimId -> Type PrimId
primTypeRef = TRef . flip Qual []

primFun :: [PrimId] -> PrimId -> Type PrimId
primFun as = TFun (map primTypeRef as) . primTypeRef

primConcat = primId "concat"
primOr = primId "or"
primAdd = primId "add"

primStr = primId "str"
primInt = primId "int"
primBool = primId "bool"

primOps :: [(PrimId, Type PrimId)]
primOps =
  [ (primConcat, primFun [primStr, primStr] primStr)
  , (primOr, primFun [primBool, primBool] primBool)
  ]

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

primRef :: (FromPrimId a) => PrimId -> Expr a
primRef = ERef . flip Qual [] . fromPrimId

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

getStr :: Expr a -> Text
getStr (ELit (LitStr xs)) = xs
getStr _ = impossible "Unexpected value in getStr"

getInt :: Expr a -> Integer
getInt (ELit (LitInt n)) = n
getInt _ = impossible "Unexpected value in getInt"

getBool :: Expr a -> Bool
getBool (ELit (LitBool n)) = n
getBool _ = impossible "Unexpected value in getBool"

twoArgs :: (a -> a -> b) -> [a] -> b
twoArgs f [x, y] = f x y
twoArgs _ _ = impossible "Wrong number of args in twoArgs"

type PrimOpImpl a = [Expr a] -> Expr a

primOpImpls :: [(PrimId, PrimOpImpl a)]
primOpImpls =
  [ (primConcat, implConcat)
  , (primOr,     implOr)
  , (primAdd,    implAdd)
  ]

implConcat, implOr :: PrimOpImpl a
implConcat = ELit . LitStr . twoArgs (<>) . map getStr
implOr = ELit . LitBool . twoArgs (||) . map getBool
implAdd = ELit . LitInt . twoArgs (+) . map getInt

evalPrim :: PrimId -> [Expr a] -> Expr a
evalPrim id args =
  maybe (impossible "Missing primOpImpl") ($ args)
  $ lookup id primOpImpls

