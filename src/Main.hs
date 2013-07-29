
module Main where

import H.Common
import H.Phase

import Lexer
import Monad
import Parser

phases :: FileMap Text -> M ()
phases =
  lexPhase
  >=> parsePhase
  >=> const (return ())

options :: MainOptions APhases AErrType Identity
options =
  MainOptions
  { moPipeline = phases
  , moName = "sb"
  , moVersion = "0.0.1"
  , moRunMonad = return . runIdentity
  }

main :: IO ()
main = phasedMain options
 
