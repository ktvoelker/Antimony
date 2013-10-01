
module Parser where

import qualified Data.Map as M
import H.Common
import H.Parser
import Text.Parsec hiding (parse, many, (<|>), optional)

import Lexer
import Monad
import Syntax

type AParser = Parser AIdClass

parsePhase :: FileMap ATokens -> M (RecBody Id)
parsePhase = stage AParse . fmap (M.fromList . M.elems) . parse file

accessMode :: AParser Access
accessMode = option Private (kw "public" *> pure Public <|> kw "extern" *> pure Extern)

ident :: AParser Id
ident = Id . snd <$> anyIdentifier

file :: AParser (Id, (Access, (Type, Expr Id)))
file = withAccess $ recBinding fileBody

recBinding :: AParser (RecBody Id) -> AParser (Id, (Type, Expr Id))
recBinding bodyParser = f <$> (kw "rec" *> ident) <*> bodyParser
  where
    f id body = (id, (TRec, ERec body))

fileBody :: AParser (RecBody Id)
fileBody =
  M.fromList <$> (kw ";" *> many binding)
  <|> 
  recBody

recBody :: AParser (RecBody Id)
recBody = M.fromList <$> delimit "{" "}" (many binding)

withAccess :: AParser (a, b) -> AParser (a, (Access, b))
withAccess p = f <$> accessMode <*> p
  where
    f acc (a, b) = (a, (acc, b))

binding :: AParser (Id, (Access, (Type, Expr Id)))
binding = withAccess $ recBinding recBody <|> valBinding

valBinding :: AParser (Id, (Type, Expr Id))
valBinding = f <$> ident <*> (fnWithType <|> constWithType)
  where
    f id te = (id, te)

fnWithType :: AParser (Type, Expr Id)
fnWithType = f <$> fnParams <*> (kw "::" *> typ) <*> valBody
  where
    f (pNames, pTypes) retType body = (TFun pTypes retType, EFun pNames body)

fnParams :: AParser ([Id], [Type])
fnParams = unzip <$> delimit "(" ")" (fnParam `sepBy` kw ",")

fnParam :: AParser (Id, Type)
fnParam = (,) <$> ident <*> (kw "::" *> typ)

constWithType :: AParser (Type, Expr Id)
constWithType = (,) <$> (kw "::" *> typ) <*> valBody

typ :: AParser Type
typ = f . snd <$> anyIdentifier
  where
    f "str" = TPrim PTStr
    f "int" = TPrim PTInt
    f "bool" = TPrim PTBool
    f xs = TRes (ResType xs)

valBody :: AParser (Expr Id)
valBody = resBody <|> exprBody <|> (kw ";" *> pure EExtern)

resBody :: AParser (Expr Id)
resBody = ERes . M.fromList <$> delimit "{" "}" (many attrBinding)

attrBinding :: AParser (Attr, Expr Id)
attrBinding = (,) <$> attrName <*> (kw "=" *> expr <* kw ";")

attrName :: AParser Attr
attrName = Attr . snd <$> anyIdentifier

exprBody :: AParser (Expr Id)
exprBody = kw "=" *> expr <* kw ";"

expr :: AParser (Expr Id)
expr = exprLit <|> exprStr <|> exprRef

exprLit :: AParser (Expr Id)
exprLit = ELit <$> (LitInt <$> litInt <|> LitBool <$> litBool)

emptyStr :: Expr Id
emptyStr = ELit (LitStr "")

mkAppend :: Expr Id -> Expr Id -> Expr Id
mkAppend a b = EApp (EPrim PrimConcat) [a, b]

mkConcat :: [Expr Id] -> Expr Id
mkConcat = foldr mkAppend emptyStr

exprStr :: AParser (Expr Id)
exprStr = mkConcat <$> between beginString endString (many strPart)

strPart :: AParser (Expr Id)
strPart =
  (ELit . LitStr <$> stringContent)
  <|>
  (between beginInterp endInterp expr)

exprRef :: AParser (Expr Id)
exprRef = f <$> qual <*> optionMaybe fnArgs
  where
    f q Nothing = ERef q
    f q (Just args) = EApp (ERef q) args

fnArgs :: AParser [Expr Id]
fnArgs = delimit "(" ")" $ expr `sepBy` kw ","

qual :: AParser (Qual Id)
qual = Qual <$> ident <*> many (kw "." *> (snd <$> anyIdentifier))

