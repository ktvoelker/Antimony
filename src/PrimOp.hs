
module PrimOp where

primOpType :: PrimOp -> Type
primOpType PrimConcat = TFun [TPrim PTStr, TPrim PTStr] (TPrim PTStr)

