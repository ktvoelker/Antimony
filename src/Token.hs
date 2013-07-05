
module Token where

data Token =
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
  deriving (Show)

