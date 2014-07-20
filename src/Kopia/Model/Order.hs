module Kopia.Model.Order where

import System.Console.CmdArgs (Data, Typeable)

data Order
    = Oldest | Newest
    deriving (Data, Typeable, Show, Eq)
