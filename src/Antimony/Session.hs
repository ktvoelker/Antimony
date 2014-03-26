
module Antimony.Session where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Functor
import qualified Data.Text as T
import System.IO (Handle)
import System.Process

import Antimony.Types (Target(..))

targetCommand :: Target -> T.Text -> [T.Text] -> (T.Text, [T.Text])
targetCommand TargetLocal bin args = (bin, args)
targetCommand (TargetSSH host port) bin args =
  ("/usr/bin/ssh", portArgs ++ (bin : args))
  where
    portArgs = case port of
      Nothing -> []
      Just n  -> ["-p", T.pack $ show n]

runOnTarget :: Target -> T.Text -> [T.Text] -> IO T.Text
runOnTarget target bin args =
  T.pack <$> readProcess (T.unpack bin') (map T.unpack args') ""
  where
    (bin', args') = targetCommand target bin args

