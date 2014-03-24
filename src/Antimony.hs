
module Antimony where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import System.Environment (getArgs)

import Antimony.Types

todo :: a
todo = error "Not implemented"

inferEnv :: Target -> IO Env
inferEnv = todo

emptyS :: S
emptyS =
  S
  { sResources = M.empty
  }

main :: AntimonyM () -> IO ()
main m = getArgs >>= runWithArgs m

argToTarget :: String -> Target
argToTarget = todo

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

