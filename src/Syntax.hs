
module Syntax.Types where

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

data Prim = PStr Text | PInt Integer | PBool Bool deriving (Show)

data Expr =
    EPrim Primitive Expr
  | ERes  (M.Map AttrName Expr)
  | EFun  [Id] Expr -> Expr
  | ENs   (M.Map Id (Type, Expr)) Expr
  | ERef  Qual Expr
  | EApp  Expr [Expr] -> Expr
  deriving (Show)

