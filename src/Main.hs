
module Main where

import Data.String
import H.Common
import H.Common.IO

import Checker
import Derefer
import Lexer
import Monad
import Parser
import Renamer
import Sorter

phases :: FileMap Text -> M ()
phases =
  lexPhase
  >=> parsePhase
  >=> renamePhase
  >=> sortPhase
  >=> derefPhase
  >=> checkPhase
  >=> const (return ())

main :: IO ()
main = runMT $ liftIO ((map fromString <$> getArgs) >>= readFiles) >>= phases

