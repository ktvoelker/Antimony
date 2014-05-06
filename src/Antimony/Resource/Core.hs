
module Antimony.Resource.Core where

import H.Generic
import H.StringMethods
import Text.JSON

class (Eq a, Ord a, Show a, Typeable a) => Primitive a where
  depends  :: a -> [Resource]
  describe :: a -> JSObject JSValue
  identify :: a -> Text

data Resource = forall a. (Primitive a) => Resource a [Resource]
  deriving (Typeable)

instance Primitive Resource where
  depends (Resource prim rs) = depends prim <> rs
  describe (Resource prim _) = describe prim
  identify (Resource prim _) = identify prim

is :: Resource -> Resource -> Bool
is (Resource a _) (Resource b _) = typeOf a == typeOf b && identify a == identify b

instance Eq Resource where
  (Resource a as) == (Resource b bs) =
    maybe False (a ==) (cast b `asTypeOf` Just a)
    && as == bs

instance Ord Resource where
  compare (Resource a as) (Resource b bs) = case compare (typeOf a) (typeOf b) of
    EQ -> maybe undefined (compare a) (cast b `asTypeOf` Just a) <> compare as bs
    ne -> ne

instance Show Resource where
  showsPrec prec (Resource a as) =
    ("(Resource " <>)
    . showsPrec prec a
    . (" " <>)
    . showsPrec prec as
    . (")" <>)

