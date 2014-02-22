
{-# LANGUAGE TemplateHaskell #-}
module Resource where

import Data.Lens.Template
import qualified Data.Map as M
import H.Common

import Syntax

data Resource =
  Resource
  { _rName  :: Qual Text
  , _rType  :: PrimId
  , _rAttrs :: M.Map Text Lit
  } deriving (Eq, Ord, Show)

makeLenses [''Resource]

