
module Antimony where

import Control.Monad.Error
import Control.Monad.Reader

import Antimony.Env
import Antimony.Resource.Core

main :: ErrorT () (Reader Env) Resource -> IO ()
main = undefined

