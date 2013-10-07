
module Syntax where

import H.Common

newtype Id = Id { idText :: Text } deriving (Eq, Ord, Show)

data Qual a = Qual a [Text] deriving (Eq, Ord, Show)

data Type a =
    TFun [Type a] (Type a)
  | TRef (Qual a)
  | TRec
  deriving (Eq, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Show)

data PrimOp = PrimConcat deriving (Eq, Ord, Enum, Bounded, Show)

data Access = Private | Public | Extern deriving (Eq, Ord, Enum, Bounded, Show)

type Namespace a = Map a (Decl a)

data BoundExpr a = BoundExpr Access (Type a) (Maybe (Expr a))
  deriving (Eq, Show)

data Decl a =
    DNamespace Access (Namespace a)
  | DType (Map a (BoundExpr a))
  | DVal (BoundExpr a)
  deriving (Eq, Show)

newtype Attr = Attr Text deriving (Eq, Ord, Show)

data Expr a =
    ELit  Lit
  | EFun  [a] (Expr a)
  | ERef  (Qual a)
  | EPrim PrimOp
  | EApp  (Expr a) [Expr a]
  | ERes  Attr (Expr a)
  | EParseError Text
  deriving (Eq, Show)

