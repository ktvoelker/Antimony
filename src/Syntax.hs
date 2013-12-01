
module Syntax where

import H.Common

newtype Id = Id { idText :: Text } deriving (Eq, Ord, Show)

data Qual a = Qual a [Text] deriving (Eq, Ord, Show)

data Type a =
    TFun [Type a] (Type a)
  | TRef (Qual a)
  deriving (Eq, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Show)

data PrimOp = PrimConcat deriving (Eq, Ord, Enum, Bounded, Show)

data Access = Private | Public | Extern deriving (Eq, Ord, Enum, Bounded, Show)

type Namespace a = DeclMap a Decl

type DeclMap a d = [(a, (Access, d a))]

data BoundExpr a = BoundExpr (Type a) (Maybe (Expr a))
  deriving (Eq, Show)

data Decl a =
    DNamespace (Namespace a)
  | DType (DeclMap a BoundExpr)
  | DVal (BoundExpr a)
  deriving (Eq, Show)

data Expr a =
    ELit  Lit
  | EFun  [a] (Expr a)
  | ERef  (Qual a)
  | EPrim PrimOp
  | EApp  (Expr a) [Expr a]
  | ERec  [(a, (Expr a))]
  deriving (Eq, Show)

