module Main where

import Kopia.Parser
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse >>= print 
