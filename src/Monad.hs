
module Monad where

import H.Common

data AErrType =
    EMerge
  | EMergeAccess
  | EUnbound
  | ECircRef
  | EDeref
  | EType
  | EKind
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

type AErr = Err AErrType

type M = MT

