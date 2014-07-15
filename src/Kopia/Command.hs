module Kopia.Command 
    ( Order(..)
    , Action(..)
    , Command(..)
    ) where

import Kopia.Bridge (Bridge)
import System.Console.CmdArgs (Data, Typeable, Default, def)

data Order
    = Oldest | Newest
    deriving (Data, Typeable, Show, Eq)

instance Default Order where
    def = Newest

data Action
    = Test
    | Take { name :: String }
    | Record { name :: String, minutes :: Int, max :: Int }
    | List { name :: String, max :: Int, order :: Order }
    | Clear { name :: String }
    | Remove { name :: String, from :: Int, max :: Int }
    | Restore { name :: String, id :: Int }
    deriving (Data, Typeable, Show, Eq)

data Command 
    = Command Bridge Action 
    deriving (Eq, Show)
