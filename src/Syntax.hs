
module Syntax where

import H.Common

newtype Id = Id Text deriving (Eq, Ord, Show)

data Qual a = Qual a [Text] deriving (Eq, Ord, Show)

newtype Attr = Attr Text deriving (Eq, Ord, Show)

data PrimType = PTStr | PTInt | PTBool deriving (Eq, Ord, Enum, Bounded, Show)

data ResType = ResType Text deriving (Eq, Ord, Show)

data Type =
    TPrim PrimType
  | TRes ResType
  | TFun [Type] Type
  | TRec
  deriving (Eq, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Show)

data PrimOp = PrimConcat deriving (Eq, Ord, Enum, Bounded, Show)

type ResBody a = Map Attr (Expr a)

data Access = Private | Public | Extern deriving (Eq, Ord, Enum, Bounded, Show)

type RecBody a = Map a (Access, (Type, Expr a))

data Expr a =
    ELit  Lit
  | ERes  (ResBody a)
  | EFun  [a] (Expr a)
  | ERec  (RecBody a)
  | ERef  (Qual a)
  | EPrim PrimOp
  | EApp  (Expr a) [Expr a]
  | EExtern
  | EParseError Text
  deriving (Eq, Show)

