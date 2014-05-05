
module Antimony.Env where

import Antimony.Types

data Kernel = Linux | Darwin deriving (Eq, Ord, Enum, Bounded, Show)

data Distro = Ubuntu | OSX deriving (Eq, Ord, Enum, Bounded, Show)

data Env =
  Env
  { envKernel    :: (Kernel, Version)
  , envDistro    :: (Distro, Version)
  , envTags      :: [Text]
  } deriving (Eq, Ord, Show)

ubuntuT :: Env
ubuntuT = Env (Linux, Version [3, 13] "") (Ubuntu, Version [14, 4] "") []

