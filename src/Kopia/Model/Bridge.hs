module Kopia.Model.Bridge where

import System.Console.CmdArgs (Data, Typeable)

data Bridge
    = Bridge
        { getTarget        :: FilePath
        , getDestination   :: FilePath }
    deriving (Data, Typeable, Show, Eq)
