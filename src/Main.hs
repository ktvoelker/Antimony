
module Main where

import Data.String
import H.Common
import H.Common.IO

import Checker
import Derefer
import Evaluator
import Grapher
import Lexer
import Monad
import Parser
import Renamer
import Sorter

dump :: (Show a) => a -> M a
dump x = liftIO (print x) >> return x

phases :: FileMap Text -> M ()
phases =
  lexPhase
  >=> parsePhase
  >=> renamePhase
  >=> sortPhase
  >=> derefPhase
  >=> checkPhase
  >=> evalPhase
  >=> graphPhase
  >=> const (return ())

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print bnf
    xs -> runMT $ (liftIO . readFiles . map fromString $ xs) >>= phases

