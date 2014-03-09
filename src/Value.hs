
module Value where

import H.Common

import Syntax

newtype Ptr = Ptr Integer deriving (Eq, Ord, Show)

data Rec =
  Rec
  { recType :: Maybe PrimId
  , recBody :: [(Text, Val)]
  } deriving (Eq, Ord, Show)

type Heap = [(Ptr, Rec)]

data Val =
    VRec Ptr
  | VLit Lit
  | VFun [Unique] (Expr Unique)
  | VPrim PrimId
  deriving (Eq, Ord, Show)

class MonadHeap m where
  deref :: Ptr -> m (Maybe Rec)
  store :: Rec -> m Ptr

