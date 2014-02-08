
module Main where

import H.Common
import H.Phase
import System.Console.CmdTheLine
import Text.Parsec.Applicative.BNF

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

phaseCmd :: Command
phaseCmd =
  phasedCommand
  $ PhasedCommandOptions
    { oPipeline = phases
    , oRunMonad = id
    , oDefault  = ASort
    , oCommand  = "compile"
    , oDoc      = "Compile the input files"
    }

bnfCmd :: Command
bnfCmd = (term, info)
  where
    term = pure . print . parserToBNF $ file
    info = defTI
      { termName = "grammar"
      , termDoc = "Show the grammar"
      }

main :: IO ()
main =
  commandMain
  $ MainOptions
    { oName     = "antimony"
    , oVersion  = "0.0.1"
    , oCommands = [bnfCmd, phaseCmd]
    }

