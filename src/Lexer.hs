
module Lexer where

import qualified Data.Set as S
import H.Common
import H.Common.IO
import H.Lexer
import Text.Parsec

import Monad

data AIdClass = AIdClass deriving (Eq, Ord, Enum, Bounded, Show)

instance IdClass AIdClass where

type AToken = Token AIdClass

lexPhase :: [FilePath] -> M [(AToken, SourcePos)]
lexPhase = stage ALex . tokenize aLexerSpec

idPrefixChars :: S.Set Char
idPrefixChars = underscore <> alphas

idChars :: S.Set Char
idChars = idPrefixChars <> digits

aLexerSpec :: LexerSpec AIdClass
aLexerSpec =
  LexerSpec
  { sKeywords    = ["ns", "public", ",", ";", "::", "{", "}", "(", ")"]
  , sIdentifiers = [(AIdClass, idPrefixChars, idChars)]
  , sStrings
    = StringSpec
    { sStringDelim = Just '"'
    , sCharDelim = Just '\''
    , sInterpolate = Just ('{', '}')
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

