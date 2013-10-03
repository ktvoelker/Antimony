
module Parser where

import qualified Data.Map as M
import Data.String
import qualified Data.Text as T
import H.Common
import H.Parser
import Text.Parsec.Applicative hiding (Parser, parse)

import Lexer
import Monad
import Syntax

newtype Sym = Sym { unSym :: Text } deriving (Eq, Ord)

instance Show Sym where
  showsPrec _ = (++) . T.unpack . unSym

instance IsString Sym where
  fromString = Sym . fromString

type AParser = Parser Sym AIdClass

parsePhase :: FileMap ATokens -> M (RecBody Id)
parsePhase = stage AParse . fmap (M.fromList . M.elems) . parse file

accessMode :: AParser Access
accessMode =
  label "access-mode"
  $ option Private (kw "public" *> pure Public <|> kw "extern" *> pure Extern)

ident :: AParser Id
ident = label "ident" $ Id . snd <$> anyIdentifier

file :: AParser (Id, (Access, (Type, Expr Id)))
file = label "file" $ withAccess $ recBinding fileBody

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
recBody = label "rec-body" $ M.fromList <$> delimit "{" "}" (many binding)

withAccess :: AParser (a, b) -> AParser (a, (Access, b))
withAccess p = f <$> accessMode <*> p
  where
    f acc (a, b) = (a, (acc, b))

binding :: AParser (Id, (Access, (Type, Expr Id)))
binding =
  label "binding"
  $ withAccess
  $ label "rec-binding" (recBinding recBody) <|> valBinding

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
fnParam = label "param" $ (,) <$> ident <*> (kw "::" *> typ)

constWithType :: AParser (Type, Expr Id)
constWithType = (,) <$> (kw "::" *> typ) <*> valBody

typ :: AParser Type
typ = label "type" $ f . snd <$> anyIdentifier
  where
    f "str" = TPrim PTStr
    f "int" = TPrim PTInt
    f "bool" = TPrim PTBool
    f xs = TRes (ResType xs)

valBody :: AParser (Expr Id)
valBody = resBody <|> exprBody <|> (kw ";" *> pure EExtern)

resBody :: AParser (Expr Id)
resBody = label "res" $ ERes . M.fromList <$> delimit "{" "}" (many attrBinding)

attrBinding :: AParser (Attr, Expr Id)
attrBinding = (,) <$> attrName <*> (kw "=" *> expr <* kw ";")

attrName :: AParser Attr
attrName = Attr . snd <$> anyIdentifier

exprBody :: AParser (Expr Id)
exprBody = kw "=" *> expr <* kw ";"

expr :: AParser (Expr Id)
expr = label "expr" $ exprLit <|> exprStr <|> exprRef

exprLit :: AParser (Expr Id)
exprLit = label "lit" $ ELit <$> (LitInt <$> litInt <|> LitBool <$> litBool)

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
exprRef = f <$> qual <*> optional fnArgs
  where
    f q Nothing = ERef q
    f q (Just args) = EApp (ERef q) args

fnArgs :: AParser [Expr Id]
fnArgs = delimit "(" ")" $ expr `sepBy` kw ","

qual :: AParser (Qual Id)
qual = label "qual" $ Qual <$> ident <*> many (kw "." *> (snd <$> anyIdentifier))

