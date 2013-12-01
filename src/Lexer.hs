
module Lexer where

import qualified Data.Set as S
import H.Common
import H.Lexer

import Monad

data AIdClass = AIdPrim | AIdUser deriving (Eq, Ord, Enum, Bounded, Show)

instance IdClass AIdClass where

type AToken = Token AIdClass

type ATokens = Tokens AIdClass

lexPhase :: FileMap Text -> M (FileMap ATokens)
lexPhase = stage ALex . tokenize aLexerSpec

primPrefixChars :: Set Char
primPrefixChars = S.singleton '#'

idPrefixChars :: Set Char
idPrefixChars = underscore <> alphas

idChars :: Set Char
idChars = idPrefixChars <> digits

aLexerSpec :: LexerSpec AIdClass
aLexerSpec =
  LexerSpec
  { sKeywords
    = ["ns", "public", "extern", ",", ";", "::", "{", "}", "(", ")", ".", "="]
  , sIdentifiers =
    [ (AIdPrim, primPrefixChars, idChars)
    , (AIdUser, idPrefixChars, idChars)
    ]
  , sStrings
    = StringSpec
    { sStringDelim = Just '"'
    , sCharDelim = Just '\''
    , sInterpMany = Just ('{', '}')
    }
  , sInts = True
  , sNegative = Just "-"
  , sFloats = False
  , sBools = Just ("false", "true")
  , sComments
    = CommentSpec
    { sLineComment  = Just "//"
    , sBlockComment = Just ("/*", "*/")
    }
  }

