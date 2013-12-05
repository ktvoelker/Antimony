
module Checker where

import H.Common

import Monad
import Syntax

checkPhase :: Namespace Unique -> M (Namespace Unique)
checkPhase b = stage ACheck $ checkNamespace b *> pure b

checkNamespace :: Namespace Unique -> M ()
checkNamespace = undefined

