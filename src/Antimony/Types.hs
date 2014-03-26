
module Antimony.Types where

import Prelude hiding (log)

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text)
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
  { fileOwner   :: Text
  , fileMode    :: [(Role, Permissions)]
  , fileContent :: Text
  } deriving (Eq, Ord, Show)

data Version = Version [Integer] Text
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

data Target =
    TargetLocal
  | TargetSSH Text (Maybe Integer)
  deriving (Eq, Ord, Show)

data Kernel = Linux | Darwin deriving (Eq, Ord, Enum, Bounded, Show)

data Distro = Ubuntu | OSX deriving (Eq, Ord, Enum, Bounded, Show)

data Env =
  Env
  { envKernel    :: (Kernel, Version)
  , envDistro    :: (Distro, Version)
  , envTarget    :: Target
  , envTags      :: [Text]
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
  say :: Text -> m ()
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

