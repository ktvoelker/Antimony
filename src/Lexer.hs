
module Lexer where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Token

tokenize :: String -> T.Text -> Either ParseError [(Token, SourcePos)]
tokenize = parse file

file :: Parser [(Token, SourcePos)]

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

