
module Main where

import System.Environment (getArgs)

import Antimony (argToEnv)

main :: IO ()
main = getArgs >>= mapM argToEnv >>= mapM_ print

