
module Antimony.Resource.Primitives where

import H.Generic

import Antimony.Resource.Core
import Antimony.Types

data Role = RoleOwner | RoleGroup | RoleOther
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

data Permissions =
  Permissions
  { canRead    :: Bool
  , canWrite   :: Bool
  , canExecute :: Bool
  } deriving (Eq, Ord, Bounded, Show, Typeable)

data FileState =
  FileState
  { fileOwner   :: Text
  , fileMode    :: [(Role, Permissions)]
  } deriving (Eq, Ord, Show, Typeable)

data Directory =
  RootDirectory
  | Directory Text FileState
  deriving (Eq, Ord, Show, Typeable)

data File = File Directory Text FileState Text
  deriving (Eq, Ord, Show, Typeable)

data Package = Package Text (Maybe VersionConstraint)
  deriving (Eq, Ord, Show, Typeable)

data Service = Service Text Bool
  deriving (Eq, Ord, Show, Typeable)

data Group = Group Text [Resource]
  deriving (Eq, Ord, Show, Typeable)

