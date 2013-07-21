
module Parser where

import H.Common
import H.Parser
-- import Text.Parsec hiding (parse)

import Lexer
import Monad
import Syntax ()

type AParser = Parser AIdClass

type AParserInput = ParserInput AIdClass

parsePhase :: AParserInput -> M ()
parsePhase = stage AParse . parse file "unknown"

-- TODO
file :: AParser ()
file = undefined

