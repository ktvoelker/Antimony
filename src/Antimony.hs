
module Antimony where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import System.Environment (getArgs)

import Antimony.Session
import Antimony.Types

todo :: a
todo = error "Not implemented"

parseVersion :: T.Text -> Version
parseVersion xs =
  Version (map (read . T.unpack) intParts) (T.intercalate "." otherParts)
  where
    parts = T.splitOn "." xs
    (intParts, otherParts) = span (T.all isDigit) parts

parseLinuxDistro :: T.Text -> T.Text -> (T.Text, Version)
parseLinuxDistro osVersion motd = todo

inferEnv :: Target -> IO Env
inferEnv t = do
  machineName <- runOnTarget t "uname" ["-m"]
  cpuArch <- runOnTarget t "uname" ["-p"]
  osRelease <- runOnTarget t "uname" ["-r"]
  osName <- runOnTarget t "uname" ["-s"]
  osVersion <- runOnTarget t "uname" ["-v"]
  motd <- runOnTarget t "head" ["-n", "1", "/etc/motd"]
  return $ case osName of
    "Darwin" ->
      Env
      { envKernel = (osName, parseVersion osRelease)
      , envDistro = ("MacOS X", parseVersion osRelease)
      , envTarget = t
      , envTags   = todo
      }
    "Linux" ->
      Env
      { envKernel = (osName, parseVersion osRelease)
      , envDistro = parseLinuxDistro osVersion motd
      , envTarget = t
      , envTags   = todo
      }

emptyS :: S
emptyS =
  S
  { sResources = M.empty
  }

main :: AntimonyM () -> IO ()
main m = getArgs >>= runWithArgs m

argToTarget :: String -> Target
argToTarget "localhost" = TargetLocal
argToTarget xs = TargetSSH (T.pack host) port
  where
    (host, portString) = break (== ':') xs
    port = case portString of
      "" -> Nothing
      xs -> Just $ read xs

planWithEnv :: AntimonyM () -> Env -> IO Plan
planWithEnv m env =
  liftM (Plan (envTarget env) . sResources)
  . runLogM
  . flip execStateT emptyS
  . flip runReaderT env
  $ runAntimony m

runPlan :: Plan -> IO ()
runPlan = todo

runWithArgs :: AntimonyM () -> [String] -> IO ()
runWithArgs m =
  -- TODO infer in parallel
  mapM (inferEnv . argToTarget)
  -- TODO plan in parallel
  >=> mapM (planWithEnv m)
  -- TODO run plans in parallel
  >=> mapM_ runPlan

