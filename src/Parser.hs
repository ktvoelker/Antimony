
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

parsePhase :: FileMap ATokens -> M (FileMap (RecBody Id))
parsePhase = stage AParse . parse file

accessMode :: AParser Access
accessMode =
  label "access-mode"
  $ option Private (kw "public" *> pure Public <|> kw "extern" *> pure Extern)

ident :: AParser Id
ident = label "ident" $ Id . snd <$> anyIdentifier

file :: AParser (RecBody Id)
file = label "file" recBody

recBody :: AParser (RecBody Id)
recBody = label "rec-body" $ M.fromList <$> delimit "{" "}" (many decl)

decl :: AParser (Id, Decl Id)
decl = declType <|> declVal

declType :: AParser (Id, Decl Id)
declType = label "decl-type" $ (,) <$> (kw "type" *> ident) <*> (DType <$> tyBody)

tyBody :: AParser (Map Id (BoundExpr Id))
tyBody = label "ty-body" $ M.fromList <$> delimit "{" "}" (many boundExpr)

declVal :: AParser (Id, Decl Id)
declVal = label "decl-val" $ f <$> boundExpr
  where
    f (id, be) = (id, DVal be)

boundExpr :: AParser (Id, BoundExpr Id)
boundExpr = label "bound-expr" $ f <$> accessMode <*> valBinding
  where
    f access (id, (ty, exp)) = (id, BoundExpr access ty exp)

valBinding :: AParser (Id, (Type Id, Maybe (Expr Id)))
valBinding = f <$> ident <*> (fnWithType <|> constWithType)
  where
    f id te = (id, te)

fnWithType :: AParser (Type Id, Maybe (Expr Id))
fnWithType = f <$> fnParams <*> (kw "::" *> typ) <*> valBody
  where
    f (pNames, pTypes) retType body = (TFun pTypes retType, Just $ EFun pNames body)

fnParams :: AParser ([Id], [Type Id])
fnParams = unzip <$> delimit "(" ")" (fnParam `sepBy` kw ",")

fnParam :: AParser (Id, Type Id)
fnParam = label "param" $ (,) <$> ident <*> (kw "::" *> typ)

constWithType :: AParser (Type Id, Maybe (Expr Id))
constWithType = (,) <$> (kw "::" *> typ) <*> ((Just <$> valBody) <|> emptyBody)

typ :: AParser (Type Id)
typ = label "type" $ TRef <$> qual

valBody :: AParser (Expr Id)
valBody = (ERec <$> recBody) <|> exprBody

emptyBody :: AParser (Maybe (Expr Id))
emptyBody = kw ";" *> pure Nothing

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

