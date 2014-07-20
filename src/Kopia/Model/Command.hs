module Kopia.Model.Command 
    ( Order(..)
    , Action(..)
    , Command(..)
    ) where

import Kopia.Model.Bridge (Bridge)
import Kopia.Model.Order (Order(..))
import System.Console.CmdArgs (Data, Typeable, Default, def)

instance Default Order where
    def = Newest

data Action
    = Test
    | Take      { getEvent :: String }
    | Record    { getEvent :: String, getMinutes :: Int, getMax :: Int }
    | List      { getEvent :: String, getMax :: Int, getOrder :: Order }
    | Clear     { getEvent :: String }
    | Remove    { getEvent :: String, getStartID :: Int, getMax :: Int }
    | Restore   { getEvent :: String, getID :: Int }
    deriving (Data, Typeable, Show, Eq)

data Command 
    = Command 
        { getBridge :: Bridge 
        , getAction :: Action }
    deriving (Eq, Show)
