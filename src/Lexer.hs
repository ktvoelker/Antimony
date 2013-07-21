
module Lexer where

import H.Common
import H.Lexer
import H.Phase
import Text.Parsec

import Monad

data AIdClass = AIdClass deriving (Eq, Ord, Enum, Bounded, Show)

instance IdClass AIdClass where

type AToken = Token AIdClass

-- TODO
lexPhase :: [InputFile] -> M [(AToken, SourcePos)]
lexPhase = stage ALex . \((InputFile name xs) : _) -> tokenize aLexerSpec name xs

idPrefixChars = '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

idChars = idPrefixChars ++ ['0' .. '9']

aLexerSpec :: LexerSpec AIdClass
aLexerSpec =
  LexerSpec
  { sKeywords    = undefined
  , sIdentifiers = [(AIdClass, idPrefixChars, idChars)]
  , sStrings
    = StringSpec
    { sStringDelim = Just '"'
    , sCharDelim = Just '\''
    , sInterpolate = undefined
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

{-
    TokNs
  | TokPriv
  | TokSep
  | TokEnd
  | TokType
  | TokLB
  | TokRB
  | TokLP
  | TokRP
  | TokId
  | TokQual
  | TokStr
  | TokInt
  | TokBool
-}

