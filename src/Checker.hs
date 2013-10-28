
module Checker where

import H.Common

import Monad
import Syntax

checkPhase :: Namespace Unique -> M (Namespace Unique)
checkPhase b = stage ACheck $ check b *> pure b

check :: Namespace Unique -> M ()
check = undefined

