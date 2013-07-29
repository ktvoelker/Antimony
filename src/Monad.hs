
module Monad where

import H.Common

data AErrType =
    EUnbound
  | ECircRef
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

type AErr = Err AErrType

data APhases =
    ALex
  | AParse
  deriving (Eq, Ord, Enum, Bounded)

instance Show APhases where
  showsPrec _ = (++) . \case
    ALex    -> "lex"
    AParse  -> "parse"

instance StageNames APhases where

type M = MT APhases AErrType Identity

