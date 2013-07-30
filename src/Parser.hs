
module Parser where

import qualified Data.Map as M
import H.Common
import H.Parser
import Text.Parsec hiding (parse, many, (<|>))

import Lexer
import Monad
import Syntax

type AParser = Parser AIdClass

parsePhase :: FileMap ATokens -> M NsBody
parsePhase = stage AParse . fmap (M.fromList . M.elems) . parse file

public :: AParser Bool
public = option False (kw "public" *> pure True)

ident :: AParser Id
ident = Id . snd <$> anyIdentifier

file :: AParser (Id, (Bool, (Type, Expr)))
file = nsBinding fileBody

nsBinding :: AParser NsBody -> AParser (Id, (Bool, (Type, Expr)))
nsBinding bodyParser = f <$> public <* kw "ns" <*> ident <*> bodyParser
  where
    f pub id body = (id, (pub, (TNs, ENs body)))

fileBody :: AParser NsBody
fileBody =
  M.fromList <$> (kw ";" *> many binding)
  <|> 
  nsBody

nsBody :: AParser NsBody
nsBody = M.fromList <$> delimit "{" "}" (many binding)

binding :: AParser (Id, (Bool, (Type, Expr)))
binding = nsBinding nsBody <|> undefined 

