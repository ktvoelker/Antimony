
module Antimony.Session where

import H.Common hiding (ExitCode)

import qualified Data.Text as T
import System.Exit
import System.Process

import Antimony.Types (Target(..))

targetCommand :: Target -> T.Text -> [T.Text] -> (T.Text, [T.Text])
targetCommand TargetLocal bin args = (bin, args)
targetCommand (TargetSSH host port) bin args =
  ("/usr/bin/ssh", [host] ++ portArgs ++ (bin : args))
  where
    portArgs = case port of
      Nothing -> []
      Just n  -> ["-p", T.pack $ show n]

runOnTarget :: Target -> T.Text -> [T.Text] -> IO (ExitCode, T.Text, T.Text)
runOnTarget target bin args =
  f <$> readProcessWithExitCode (T.unpack bin') (map T.unpack args') ""
  where
    (bin', args') = targetCommand target bin args
    f (code, out, err) = (code, T.pack out, T.pack err)

