
module Antimony.Env where

import qualified Data.Text as T

import Antimony.Types

data Kernel = Linux | Darwin deriving (Eq, Ord, Enum, Bounded, Show)

data Distro = Ubuntu | OSX deriving (Eq, Ord, Enum, Bounded, Show)

data Env =
  Env
  { envKernel    :: (Kernel, Version)
  , envDistro    :: (Distro, Version)
  , envTags      :: [T.Text]
  } deriving (Eq, Ord, Show)

