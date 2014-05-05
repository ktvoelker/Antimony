
module Antimony.Types where

import H.Generic

data Version = Version [Integer] Text
  deriving (Eq, Ord, Show, Typeable)

type VersionConstraint = (Ordering, Version)

