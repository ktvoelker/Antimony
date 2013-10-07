
module Sorter where

import H.Common

import Monad
import Syntax

sortPhase :: Namespace Unique -> M (Namespace Unique)
sortPhase = stage ASort . undefined

