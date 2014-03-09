
module Syntax where

import H.Common

data Id = Id Text | Prim PrimId deriving (Eq, Ord, Show)

instance FromPrimId Id where
  fromPrimId = Prim

data Qual a = Qual a [Text] deriving (Eq, Ord, Show)

data Type a =
    TFun [Type a] (Type a)
  | TRef (Qual a)
  deriving (Eq, Ord, Show)

data Lit = LitStr Text | LitInt Integer | LitBool Bool deriving (Eq, Ord, Show)

data Access = Private | Public | Extern deriving (Eq, Ord, Enum, Bounded, Show)

type Namespace a = DeclMap a Decl

type DeclMapElem a d = (a, (Access, d a))

type DeclMap a d = [DeclMapElem a d]

data BoundExpr a =
  BoundExpr
  { boundExprType  :: Type a
  , boundExprValue :: Maybe (Expr a)
  } deriving (Eq, Ord, Show)

data Decl a =
    DNamespace (Namespace a)     -- ^ A namespace
  | DType (DeclMap a BoundExpr)  -- ^ A record type
  | DVal (BoundExpr a)           -- ^ A value binding
  | DPrim PrimId                 -- ^ A primitive type
  deriving (Eq, Ord, Show)

data Expr a =
    ELit  Lit
  | EFun  [a] (Expr a)
  | ERef  (Qual a)
  | EApp  (Expr a) [Expr a]
  | ERec  (Maybe PrimId) [(a, (Expr a))]
  | EPrim PrimId
  deriving (Eq, Ord, Show)

