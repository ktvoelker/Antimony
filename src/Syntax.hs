
module Syntax where

import H.Common

newtype Id = Id Text deriving (Eq, Ord, Show)

newtype Qual = Qual [Id] deriving (Eq, Ord, Show)

newtype Attr = Attr Text deriving (Eq, Ord, Show)

data PrimType = PTStr | PTInt | PTBool deriving (Eq, Ord, Enum, Bounded, Show)

data ResType = ResType Text deriving (Eq, Ord, Show)

data Type =
    TPrim PrimType
  | TRes ResType
  | TFun [Type] Type
  | TNs
  deriving (Eq, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Show)

data Prim = PrimConcat deriving (Eq, Ord, Enum, Bounded, Show)

type ResBody = Map Attr Expr

type NsBody = Map Id (Bool, (Type, Expr))

data Expr =
    ELit  Lit
  | ERes  ResBody
  | EFun  [Id] Expr
  | ENs   NsBody
  | ERef  Qual
  | EPrim Prim
  | EApp  Expr [Expr]
  | EParseError Text
  deriving (Eq, Show)

