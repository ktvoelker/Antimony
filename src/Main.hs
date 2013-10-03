
module Main where

import H.Common
import H.Phase
import System.Console.CmdTheLine
import Text.Parsec.Applicative.BNF

import Checker
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
  >=> checkPhase
  >=> const (return ())

options :: MainOptions APhases AErrType Identity
options =
  MainOptions
  { moPipeline = phases
  , moName = "antimony"
  , moVersion = "0.0.1"
  , moRunMonad = return . runIdentity
  , moChoices = [(bnf, bnfTI)]
  }

bnf :: Term (IO ())
bnf = pure . print . parserToBNF $ file

bnfTI :: TermInfo
bnfTI = defTI
  { termName = "grammar"
  , termDoc = "Show the grammar"
  }

main :: IO ()
main = phasedMain options
 
