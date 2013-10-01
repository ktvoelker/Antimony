
module Checker where

import H.Common

import Monad
import Syntax

checkPhase :: RecBody Unique -> M (RecBody Unique)
checkPhase b = stage ACheck $ check b *> pure b

check :: RecBody Unique -> M ()
check = undefined

