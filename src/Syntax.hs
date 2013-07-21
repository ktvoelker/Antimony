
module Syntax where

import H.Common
import qualified Data.Map as M
import qualified Data.Text as T

newtype Id = Id T.Text deriving (Show)

newtype Qual = Qual [Id] deriving (Show)

newtype Attr = Attr T.Text deriving (Show)

data PrimType = PTStr | PTInt | PTBool deriving (Show)

data ResType = ResType T.Text deriving (Show)

data Type =
    TPrim PrimType
  | TRes ResType
  | TFun [Type] Type
  | TNs
  deriving (Show)

data Prim = PStr T.Text | PInt Integer | PBool Bool deriving (Show)

data Expr =
    EPrim Prim
  | ERes  (M.Map Attr Expr)
  | EFun  [Id] Expr
  | ENs   (M.Map Id (Type, Expr))
  | ERef  Qual
  | EApp  Expr [Expr]
  deriving (Show)

