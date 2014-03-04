
module Prim.Eval (evalPrim) where

import H.Common

import Prim
import Value
import Syntax

getStr :: Val -> Text
getStr (VLit (LitStr xs)) = xs
getStr _ = impossible "Unexpected value in getStr"

getInt :: Val -> Integer
getInt (VLit (LitInt n)) = n
getInt _ = impossible "Unexpected value in getInt"

getBool :: Val -> Bool
getBool (VLit (LitBool n)) = n
getBool _ = impossible "Unexpected value in getBool"

twoArgs :: (a -> a -> b) -> [a] -> b
twoArgs f [x, y] = f x y
twoArgs _ _ = impossible "Wrong number of args in twoArgs"

type PrimOpImpl = [Val] -> Val

primOpImpls :: [(PrimId, PrimOpImpl)]
primOpImpls =
  [ (primConcat, implConcat)
  , (primOr,     implOr)
  , (primAdd,    implAdd)
  ]

implConcat, implOr :: PrimOpImpl
implConcat = VLit . LitStr . twoArgs (<>) . map getStr
implOr = VLit . LitBool . twoArgs (||) . map getBool
implAdd = VLit . LitInt . twoArgs (+) . map getInt

evalPrim :: PrimId -> [Val] -> Val
evalPrim id args =
  maybe (impossible "Missing primOpImpl") ($ args)
  $ lookup id primOpImpls

