
module Parser where

import Data.String
import qualified Data.Text as T
import H.Common
import H.Parser
import Text.Parsec.Applicative hiding (Parser, parse)

import Lexer
import Monad
import Prim
import Syntax

newtype Sym = Sym { unSym :: Text } deriving (Eq, Ord)

instance Show Sym where
  showsPrec _ = (++) . T.unpack . unSym

instance IsString Sym where
  fromString = Sym . fromString

type AParser = Parser Sym AIdClass

parsePhase :: FileMap ATokens -> M (FileMap (Namespace Id))
parsePhase = checked . parse file

accessMode :: AParser Access
accessMode =
  label "access-mode"
  $ option Private (kw "public" *> pure Public <|> kw "extern" *> pure Extern)

ident :: AParser Id
ident = label "ident" $ f <$> anyIdentifier
  where
    f (AIdPrim, xs) = Prim . primId . T.tail $ xs
    f (AIdUser, xs) = Id xs

userIdent :: AParser Id
userIdent = label "user-ident" $ Id <$> identifier AIdUser

file :: AParser (Namespace Id)
file = label "file" $ (: []) <$> withAccess fileNamespace

namespace
  :: AParser Id
  -> AParser [(Id, (Access, Decl Id))]
  -> AParser (Id, Decl Id)
namespace pHeader pBody = f <$> pHeader <*> pBody
  where
    f nsName body = (nsName, DNamespace body)

fileNamespace :: AParser (Id, Decl Id)
fileNamespace = label "file-ns" $ namespace (nsHeader <* kw ";") nsBody

nestedNamespace :: AParser (Id, Decl Id)
nestedNamespace = label "decl-ns" $ namespace nsHeader $ delimit "{" "}" nsBody

nsHeader :: AParser Id
nsHeader = kw "ns" *> userIdent

nsBody :: AParser [(Id, (Access, Decl Id))]
nsBody = many decl

withAccess :: AParser (a, b) -> AParser (a, (Access, b))
withAccess p = f <$> accessMode <*> p
  where
    f access (a, b) = (a, (access, b))

decl :: AParser (Id, (Access, Decl Id))
decl = label "decl" $ withAccess $ nestedNamespace <|> declType <|> declVal

declType :: AParser (Id, Decl Id)
declType = label "decl-type" $ (,) <$> (kw "type" *> userIdent) <*> (DType <$> tyBody)

tyBody :: AParser [(Id, (Access, BoundExpr Id))]
tyBody = delimit "{" "}" . many . withAccess $ boundExpr

declVal :: AParser (Id, Decl Id)
declVal = label "decl-val" $ f <$> boundExpr
  where
    f (id, be) = (id, DVal be)

boundExpr :: AParser (Id, BoundExpr Id)
boundExpr = label "bound-expr" $ f <$> valBinding
  where
    f (id, (ty, exp)) = (id, BoundExpr ty exp)

valBinding :: AParser (Id, (Type Id, Maybe (Expr Id)))
valBinding = f <$> userIdent <*> (fnWithType <|> constWithType)
  where
    f id te = (id, te)

fnWithType :: AParser (Type Id, Maybe (Expr Id))
fnWithType = f <$> fnParams <*> (kw "::" *> typ) <*> exprBody
  where
    f (pNames, pTypes) retType body = (TFun pTypes retType, Just $ EFun pNames body)

fnParams :: AParser ([Id], [Type Id])
fnParams = unzip <$> delimit "(" ")" (fnParam `sepBy` kw ",")

fnParam :: AParser (Id, Type Id)
fnParam = label "param" $ (,) <$> userIdent <*> (kw "::" *> typ)

constWithType :: AParser (Type Id, Maybe (Expr Id))
constWithType = (,) <$> (kw "::" *> typ) <*> ((Just <$> exprBody) <|> emptyBody)

typ :: AParser (Type Id)
typ = label "type" $ TRef <$> qual

emptyBody :: AParser (Maybe (Expr Id))
emptyBody = kw ";" *> pure Nothing

exprBody :: AParser (Expr Id)
exprBody = kw "=" *> expr <* kw ";"

expr :: AParser (Expr Id)
expr = label "expr" $ exprLit <|> exprStr <|> exprRef <|> exprRec

exprLit :: AParser (Expr Id)
exprLit = label "lit" $ ELit <$> (LitInt <$> litInt <|> LitBool <$> litBool)

emptyStr :: Expr Id
emptyStr = ELit (LitStr "")

mkAppend :: Expr Id -> Expr Id -> Expr Id
mkAppend a b = EApp (EPrim primConcat) [a, b]

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

exprRec :: AParser (Expr Id)
exprRec = label "rec" $ ERec <$> recBody

recBody :: AParser [(Id, (Expr Id))]
recBody = delimit "{" "}" . many $ recBind

recBind :: AParser (Id, Expr Id)
recBind = (,) <$> userIdent <*> (kw "=" *> expr <* kw ";")

fnArgs :: AParser [Expr Id]
fnArgs = delimit "(" ")" $ expr `sepBy` kw ","

qual :: AParser (Qual Id)
qual = label "qual" $ Qual <$> ident <*> many (kw "." *> (snd <$> anyIdentifier))

