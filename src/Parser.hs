
module Parser where

import qualified Data.Map as M
import H.Common
import H.Parser
import Text.Parsec hiding (parse, many, (<|>), optional)

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
binding = nsBinding nsBody <|> valBinding

valBinding :: AParser (Id, (Bool, (Type, Expr)))
valBinding = f <$> public <*> ident <*> (fnWithType <|> constWithType)
  where
    f pub id te = (id, (pub, te))

fnWithType :: AParser (Type, Expr)
fnWithType = f <$> fnParams <*> (kw "::" *> typ) <*> valBody
  where
    f (pNames, pTypes) retType body = (TFun pTypes retType, EFun pNames body)

fnParams :: AParser ([Id], [Type])
fnParams = unzip <$> delimit "(" ")" (fnParam `sepBy` kw ",")

fnParam :: AParser (Id, Type)
fnParam = (,) <$> ident <*> typ

constWithType :: AParser (Type, Expr)
constWithType = (,) <$> (kw "::" *> typ) <*> valBody

typ :: AParser Type
typ = f . snd <$> anyIdentifier
  where
    f "str" = TPrim PTStr
    f "int" = TPrim PTInt
    f "bool" = TPrim PTBool
    f xs = TRes (ResType xs)

valBody :: AParser Expr
valBody = resBody <|> exprBody

resBody :: AParser Expr
resBody = ERes . M.fromList <$> delimit "{" "}" (many attrBinding)

attrBinding :: AParser (Attr, Expr)
attrBinding = (,) <$> attrName <*> (kw "=" *> expr <* kw ";")

attrName :: AParser Attr
attrName = Attr . snd <$> anyIdentifier

exprBody :: AParser Expr
exprBody = kw "=" *> expr <* kw ";"

expr :: AParser Expr
expr = exprLit <|> exprStr <|> exprRef

exprLit :: AParser Expr
exprLit = ELit <$> (LitInt <$> litInt <|> LitBool <$> litBool)

emptyStr :: Expr
emptyStr = ELit (LitStr "")

mkAppend :: Expr -> Expr -> Expr
mkAppend a b = EApp (EPrim PrimConcat) [a, b]

mkConcat :: [Expr] -> Expr
mkConcat = foldr mkAppend emptyStr

exprStr :: AParser Expr
exprStr = mkConcat <$> between beginString endString (many strPart)

strPart :: AParser Expr
strPart =
  (ELit . LitStr <$> stringContent)
  <|>
  (between beginInterp endInterp expr)

exprRef :: AParser Expr
exprRef = f <$> qual <*> optionMaybe fnArgs
  where
    f q Nothing = ERef q
    f q (Just args) = EApp (ERef q) args

fnArgs :: AParser [Expr]
fnArgs = delimit "(" ")" $ expr `sepBy` kw ","

qual :: AParser Qual
qual = Qual <$> ident `sepBy` kw "."

