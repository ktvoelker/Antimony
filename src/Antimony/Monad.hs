
module Antimony.Monad where

import H.StringMethods

import Antimony.Env

newtype Err = Err Text
  deriving (Eq, Ord, Read, Show)

instance Error Err where
  noMsg = Err "Unknown"
  strMsg = Err . pack

newtype Sb a = Sb { unSb :: ErrorT Err (Reader Env) a }
  deriving (Functor, Applicative, Monad)

runSb :: Env -> Sb a -> Either Err a
runSb env = flip runReader env . runErrorT . unSb

