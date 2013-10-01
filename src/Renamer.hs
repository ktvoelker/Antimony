
module Renamer where

import H.Common

import Monad
import Syntax

renamePhase :: RecBody Id -> M (RecBody Unique)
renamePhase = stage ARename . undefined

