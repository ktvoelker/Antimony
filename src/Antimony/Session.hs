
module Antimony.Session where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.IO (Handle)
import System.Process

import Antimony.Types (Target(..))

data Session =
  Session
  { sInput   :: Handle
  , sOutput  :: Handle
  , sError   :: Handle
  , sProcess :: ProcessHandle
  , sKey     :: ReleaseKey
  }

rel :: (Handle, Handle, Handle, ProcessHandle) -> IO ()
rel (_, _, _, p) = terminateProcess p

startSessionWithCommand :: String -> [String] -> ResourceT IO Session
startSessionWithCommand bin args = do
  let rip = runInteractiveProcess bin args Nothing Nothing
  (rk, (i, o, e, p)) <- allocate rip rel
  return $ Session i o e p rk

startSession :: Target -> ResourceT IO Session
startSession = uncurry startSessionWithCommand . targetToCommand

targetToCommand :: Target -> (String, [String])
targetToCommand TargetLocal = ("/bin/sh", [])
targetToCommand (TargetSSH host port) = ("/usr/bin/ssh", portArgs ++ ["/bin/sh"])
  where
    portArgs = case port of
      Nothing -> []
      Just n  -> ["-p", show n]

stopSession :: Session -> Resource ()
stopSession (Session _ _ _ _ rk) = release rk

withSession :: Target -> (Session -> IO a) -> IO a
withSession t f = runResourceT $ startSession t >>= liftIO . f

