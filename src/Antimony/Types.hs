
module Antimony.Types where

import H.Common

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
  { fileOwner   :: T.Text
  , fileMode    :: [(Role, Permissions)]
  , fileContent :: T.Text
  } deriving (Eq, Ord, Show)

data Version = Version [Integer] T.Text
  deriving (Eq, Ord, Show)

type VersionConstraint = (Ordering, Version)

data Primitive =
    File T.Text FileState
  | Package T.Text (Maybe VersionConstraint)
  | Service T.Text Bool
  | Group
  deriving (Eq, Ord, Show)

newtype Resource = Resource T.Text
  deriving (Eq, Ord, Show)

data Target =
    TargetLocal
  | TargetSSH T.Text (Maybe Integer)
  deriving (Eq, Ord, Show)

data Kernel = Linux | Darwin deriving (Eq, Ord, Enum, Bounded, Show)

data Distro = Ubuntu | OSX deriving (Eq, Ord, Enum, Bounded, Show)

data Env =
  Env
  { envKernel    :: (Kernel, Version)
  , envDistro    :: (Distro, Version)
  , envTarget    :: Target
  , envTags      :: [T.Text]
  } deriving (Eq, Ord, Show)

type ResourceMap = M.Map Resource (Primitive, [Resource])

data Plan = Plan Target ResourceMap
  deriving (Eq, Ord, Show)

data S =
  S
  { sResources :: ResourceMap
  } deriving (Eq, Ord, Show)

newtype LogM a = LogM { runLogM :: IO a }

instance Monad LogM where
  (LogM io) >>= f = LogM $ io >>= runLogM . f
  return = LogM . return
  fail = LogM . fail

class MonadLog m where
  say :: T.Text -> m ()
  log :: (Show a) => a -> m ()

instance MonadLog IO where
  say = TIO.putStrLn
  log = print

instance MonadLog LogM where
  say = LogM . say
  log = LogM . log

newtype AntimonyM a =
  AntimonyM
  { runAntimony :: (ReaderT Env (StateT S LogM)) a
  }

