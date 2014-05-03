
module Antimony.Resource.Primitives where

import qualified Data.Text as T
import Data.Typeable

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
  { fileOwner   :: T.Text
  , fileMode    :: [(Role, Permissions)]
  } deriving (Eq, Ord, Show, Typeable)

data Directory =
  RootDirectory
  | Directory T.Text FileState
  deriving (Eq, Ord, Show, Typeable)

data File = File Directory T.Text FileState T.Text
  deriving (Eq, Ord, Show, Typeable)

data Package = Package T.Text (Maybe VersionConstraint)
  deriving (Eq, Ord, Show, Typeable)

data Service = Service T.Text Bool
  deriving (Eq, Ord, Show, Typeable)

data Group = Group T.Text [Resource]
  deriving (Eq, Ord, Show, Typeable)

