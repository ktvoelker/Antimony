
module Antimony where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import System.Environment (getArgs)

import Antimony.Session
import Antimony.Types

todo :: a
todo = error "Not implemented"

inferEnv :: Target -> IO Env
inferEnv t = withSession t $ \s -> do
  todo

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

