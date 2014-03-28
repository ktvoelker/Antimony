
module Antimony where

import H.Common
import H.Common.IO

import qualified Data.Map as M
import qualified Data.Text as T

import Antimony.Session
import Antimony.Types

readText :: (Read a) => T.Text -> Maybe a
readText xs = case reads . T.unpack $ xs of
  [(result, "")] -> Just result
  _ -> Nothing

parseVersion :: T.Text -> Version
parseVersion xs =
  Version (map (read . T.unpack) intParts) (T.intercalate "." otherParts)
  where
    parts = T.splitOn "." xs
    (intParts, otherParts) = span (T.all isDigit) parts

parseLinuxDistro :: T.Text -> T.Text -> (Distro, Version)
parseLinuxDistro _ motd =
  case take 1 . drop 1 . dropWhile (/= "Ubuntu") . T.words $ motd of
    [version] -> (Ubuntu, parseVersion version)
    _ -> error "Unknown Linux distro"

inferEnv :: Target -> IO Env
inferEnv t = do
  osRelease <- r t "uname" ["-r"]
  osName    <- r t "uname" ["-s"]
  osVersion <- r t "uname" ["-v"]
  motd      <- r t "head" ["-n", "1", "/etc/motd"]
  return $ case osName of
    "Darwin" ->
      Env
      { envKernel = (Darwin, parseVersion osRelease)
      , envDistro = (OSX, parseVersion osRelease)
      , envTarget = t
      , envTags   = [] -- TODO
      }
    "Linux" ->
      Env
      { envKernel = (Linux, parseVersion osRelease)
      , envDistro = parseLinuxDistro osVersion motd
      , envTarget = t
      , envTags   = [] -- TODO
      }
    _ -> error $ "Unknown OS name: " ++ T.unpack osName
  where
    r t bin args = f <$> runOnTarget t bin args
    f (_, out, _) = T.strip out

emptyS :: S
emptyS =
  S
  { sResources = M.empty
  }

main :: AntimonyM () -> IO ()
main m = getArgs >>= runWithArgs m . map T.pack

argToTarget :: T.Text -> Target
argToTarget "localhost" = TargetLocal
argToTarget xs = TargetSSH host port
  where
    (host, portText) = T.break (== ':') xs
    port = case portText of
      "" -> Nothing
      xs -> maybe todo Just $ readText xs

planWithEnv :: AntimonyM () -> Env -> IO Plan
planWithEnv m env =
  liftM (Plan (envTarget env) . sResources)
  . runLogM
  . flip execStateT emptyS
  . flip runReaderT env
  $ runAntimony m

runPlan :: Plan -> IO ()
runPlan = todo

argToEnv :: T.Text -> IO Env
argToEnv = inferEnv . argToTarget

runWithArgs :: AntimonyM () -> [T.Text] -> IO ()
runWithArgs m =
  -- TODO infer in parallel
  mapM argToEnv
  -- TODO plan in parallel
  >=> mapM (planWithEnv m)
  -- TODO run plans in parallel
  >=> mapM_ runPlan

