
module Parser where

import qualified Data.Map as M
import H.Common
import H.Parser
import Text.Parsec hiding (parse, many, (<|>), optional)

import Lexer
import Monad
import Syntax

type AParser = Parser AIdClass

parsePhase :: FileMap ATokens -> M RecBody
parsePhase = stage AParse . fmap (M.fromList . M.elems) . parse file

accessMode :: AParser Access
accessMode = option Private (kw "public" *> pure Public <|> kw "extern" *> pure Extern)

ident :: AParser Id
ident = Id . snd <$> anyIdentifier

file :: AParser (Id, (Access, (Type, Expr)))
file = withAccess $ recBinding fileBody

recBinding :: AParser RecBody -> AParser (Id, (Type, Expr))
recBinding bodyParser = f <$> (kw "rec" *> ident) <*> bodyParser
  where
    f id body = (id, (TRec, ERec body))

fileBody :: AParser RecBody
fileBody =
  M.fromList <$> (kw ";" *> many binding)
  <|> 
  recBody

recBody :: AParser RecBody
recBody = M.fromList <$> delimit "{" "}" (many binding)

withAccess :: AParser (a, b) -> AParser (a, (Access, b))
withAccess p = f <$> accessMode <*> p
  where
    f acc (a, b) = (a, (acc, b))

binding :: AParser (Id, (Access, (Type, Expr)))
binding = withAccess $ recBinding recBody <|> valBinding

valBinding :: AParser (Id, (Type, Expr))
valBinding = f <$> ident <*> (fnWithType <|> constWithType)
  where
    f id te = (id, te)

fnWithType :: AParser (Type, Expr)
fnWithType = f <$> fnParams <*> (kw "::" *> typ) <*> valBody
  where
    f (pNames, pTypes) retType body = (TFun pTypes retType, EFun pNames body)

fnParams :: AParser ([Id], [Type])
fnParams = unzip <$> delimit "(" ")" (fnParam `sepBy` kw ",")

fnParam :: AParser (Id, Type)
fnParam = (,) <$> ident <*> (kw "::" *> typ)

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
exprBody = option EExtern (kw "=" *> expr) <* kw ";"

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

