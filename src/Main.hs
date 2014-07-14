module Main where

import Kopia.Parser
import Kopia.Interpreter
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse >>= execute
