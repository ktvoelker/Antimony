
module Prim where

import H.Common

primConcat = primId "concat"

primOps :: [PrimId]
primOps = [primConcat]

