
module Lexer where

import H.Common
import H.Lexer
import H.Phase
import Text.Parsec

import Monad

data AIdClass = AIdClass deriving (Eq, Ord, Enum, Bounded, Show)

instance IdClass AIdClass where

type AToken = Token AIdClass

lexPhase :: [InputFile] -> M [(AToken, SourcePos)]
lexPhase = stage ALex . tokenize aLexerSpec

idPrefixChars = '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

idChars = idPrefixChars ++ ['0' .. '9']

aLexerSpec :: LexerSpec AIdClass
aLexerSpec =
  LexerSpec
  { sKeywords    = ["ns", "public", ",", ";", "::", "{", "}", "(", ")"]
  , sIdentifiers = [(AIdClass, idPrefixChars, idChars)]
  , sStrings
    = StringSpec
    { sStringDelim = Just '"'
    , sCharDelim = Just '\''
    , sInterpolate = Just ("{", "}")
    }
  , sInts = True
  , sNegative = Just "-"
  , sFloats = True
  , sBools = Just ("false", "true")
  , sComments
    = CommentSpec
    { sLineComment  = Just "//"
    , sBlockComment = Just ("/*", "*/", True)
    }
  }

