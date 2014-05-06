
module Antimony where

import qualified Data.Set as S
import H.IO
import Text.JSON

import Antimony.Env
import Antimony.Monad
import Antimony.Resource.Core

flatten :: Sb Resource -> Sb (ListBuilder (JSObject JSValue))
flatten = fmap $ \res -> fst $ execState (go res) mempty
  where
    go :: Resource -> State (ListBuilder (JSObject JSValue), S.Set Resource) ()
    go res = whenM (gets $ not . S.member res . snd) $ do
      modify $ (<> (mempty, S.singleton res))
      mapM_ go (depends res)
      modify $ (<> (buildElem $ describe res, mempty))
      return ()

runEnv :: Env -> Sb Resource -> Either Err JSValue
runEnv env = flatten >>> runSb env >>> fmap (finishList >>> fmap JSObject >>> JSArray)

parseArgs :: [Text] -> Either Err (Env, FilePath)
parseArgs _ = Right (ubuntuT, "out.json")

runArgs :: [Text] -> Sb Resource -> Either Err (JSValue, FilePath)
runArgs xs res = parseArgs xs >>= \(env, out) -> fmap (, out) $ runEnv env res

main :: Sb Resource -> IO ()
main res = do
  xs <- getArgs
  case runArgs xs res of
    Left (Err msg) -> putStrLn msg >> exitFailure
    Right (json, out) -> writeFile out . pack . encodeStrict $ json

