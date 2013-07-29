
module Parser where

import H.Common
import H.Parser
-- import Text.Parsec hiding (parse)

import Lexer
import Monad
import Syntax ()

type AParser = Parser AIdClass

parsePhase :: FileMap ATokens -> M ()
parsePhase = stage AParse . void . parse files

-- TODO
files :: AParser ()
files = undefined

