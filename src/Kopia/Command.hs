module Kopia.Command where

import System.Console.CmdArgs (Data, Typeable)

data Bridge
    = Bridge
        { storage   :: FilePath
        , target    :: FilePath }
    deriving (Data, Typeable, Show, Eq)

data Action
    = Status
    | Take { name :: String }
    | Record { name :: String, minutes :: Int, max :: Int }
    | List { name :: String, max :: Int } -- order
    | Clear { name :: String }
    | Remove { name :: String, from :: Int, max :: Int }
    | Restore { name :: String, id :: Int }
    deriving (Data, Typeable, Show, Eq)

data Command 
    = Command Bridge Action 
    deriving (Eq, Show)
