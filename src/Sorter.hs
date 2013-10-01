
module Sorter where

import H.Common

import Monad
import Syntax

sortPhase :: RecBody Unique -> M (RecBody Unique)
sortPhase = stage ASort . undefined

