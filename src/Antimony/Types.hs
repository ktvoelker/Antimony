
module Antimony.Types where

import qualified Data.Text as T
import Data.Typeable

data Version = Version [Integer] T.Text
  deriving (Eq, Ord, Show, Typeable)

type VersionConstraint = (Ordering, Version)

