
module Antimony where

import qualified Data.Map as M
import Data.Text (Text)

data Role = RoleOwner | RoleGroup | RoleOther
  deriving (Eq, Ord, Enum, Bounded, Show)

data Permissions =
  Permissions
  { canRead    :: Bool
  , canWrite   :: Bool
  , canExecute :: Bool
  } deriving (Eq, Ord, Bounded, Show)

data FileState =
  FileState
  { fileOwner   :: Text
  , fileMode    :: [(Role, Permissions)]
  , fileContent :: Text
  } deriving (Eq, Ord, Show)

newtype Version = Version [Integer]
  deriving (Eq, Ord, Show)

type VersionConstraint = (Ordering, Version)

data Primitive =
    File Text FileState
  | Package Text (Maybe VersionConstraint)
  | Service Text Bool
  | Group
  deriving (Eq, Ord, Show)

newtype Resource = Resource Text
  deriving (Eq, Ord, Show)

data Env tag =
  Env
  { envKernel    :: (Text, Version)
  , envDistro    :: (Text, Version)
  , envTarget    :: Text
  , envTags      :: [tag]
  } deriving (Eq, Ord, Show)

data S =
  S
  { sResources :: M.Map Resource (Primitive, [Resource])
  } deriving (Eq, Ord, Show)

newtype AntimonyM tag a =
  AntimonyM
  { runAntimony :: StateT S (ReaderT (Env tag) IO) a
  }

