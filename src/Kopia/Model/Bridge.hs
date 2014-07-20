module Kopia.Model.Bridge where

import System.Console.CmdArgs (Data, Typeable)

data Bridge
    = Bridge
        { target        :: FilePath
        , destination   :: FilePath }
    deriving (Data, Typeable, Show, Eq)
