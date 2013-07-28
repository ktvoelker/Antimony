
module Main where

import H.Common
import H.Common.IO
import H.Phase

import Lexer
import Monad
import Parser

phases :: [FilePath] -> M ()
phases =
  lexPhase
  >=> parsePhase
  >=> const (return ())

options :: MainOptions APhases AErrType IO
options =
  MainOptions
  { moPipeline = phases
  , moName = "sb"
  , moVersion = "0.0.1"
  , moRunMonad = id
  }

main :: IO ()
main = phasedMain options
 
