
module Syntax where

import H.Common

data Id = Id Text | Prim PrimId deriving (Eq, Ord, Show)

data Qual a = Qual a [Text] deriving (Eq, Ord, Show)

data Type a =
    TFun [Type a] (Type a)
  | TRef (Qual a)
  deriving (Eq, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Show)

data Access = Private | Public | Extern deriving (Eq, Ord, Enum, Bounded, Show)

type Namespace a = DeclMap a Decl

type DeclMap a d = [(a, (Access, d a))]

data BoundExpr a = BoundExpr (Type a) (Maybe (Expr a))
  deriving (Eq, Show)

data Decl a =
    DNamespace (Namespace a)
  | DType (DeclMap a BoundExpr)
  | DVal (BoundExpr a)
  | DPrim PrimId
  deriving (Eq, Show)

data Expr a =
    ELit  Lit
  | EFun  [a] (Expr a)
  | ERef  (Qual a)
  | EApp  (Expr a) [Expr a]
  | ERec  [(a, (Expr a))]
  | EPrim PrimId
  deriving (Eq, Show)

