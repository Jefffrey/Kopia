module Kopia.Parser (parse) where

import Prelude hiding (max, id)
import System.Environment (withArgs)
import System.Console.CmdArgs ((&=), CmdArgs, Mode, Default)
import System.Console.CmdArgs.Explicit (process)
import Kopia.Model.Bridge (Bridge(..))
import Kopia.Model.Order (Order(..))
import Kopia.Model.Command (Command(..), Action(..))
import qualified System.Console.CmdArgs as Cmd

bridgeTemplate :: Bridge
bridgeTemplate = 
    Bridge 
        { getTarget = Cmd.def 
            &= Cmd.argPos 0
            &= Cmd.typ "TARGET" 
        , getDestination = Cmd.def 
            &= Cmd.argPos 1 
            &= Cmd.typ "DESTINATION" }
    &= Cmd.summary "Kopia - backup system"
    &= Cmd.help "Estabilish bridge"
    &= Cmd.helpArg [Cmd.explicit, Cmd.name "help", Cmd.name "h"]
    &= Cmd.program "kopia"

parseBridge :: IO Bridge
parseBridge = Cmd.cmdArgsRun $ Cmd.cmdArgsMode bridgeTemplate

testActionTemplate :: Action
testActionTemplate = Test &= Cmd.help "Tests the bridge"

takeActionTemplate :: Action
takeActionTemplate = 
    Take 
        { getEvent = Cmd.def 
            &= Cmd.argPos 0 
            &= Cmd.typ "EVENT" }
    &= Cmd.help "Takes a snapshot for an event"

recordActionTemplate :: Action
recordActionTemplate =
    Record
        { getEvent = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "EVENT"
        , getMinutes = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "MINUTES"
        , getMax = 1
            &= Cmd.name "max"
            &= Cmd.typ "MAXIMUM" }
    &= Cmd.help "Takes a serie of snapshots every MINUTES minutes"
                
listActionTemplate :: Action
listActionTemplate =
    List
        { getEvent = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "EVENT"
        , getMax = 100
            &= Cmd.name "max"
            &= Cmd.typ "MAXIMUM"
        , getOrder = Cmd.def
            &= Cmd.name "order"
            &= Cmd.typ "ORDER" }
    &= Cmd.help "Lists all the snapshots of an event"

clearActionTemplate :: Action
clearActionTemplate =
    Clear
        { getEvent = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "EVENT" }
    &= Cmd.help "Deletes every snapshot of an event"

removeActionTemplate :: Action
removeActionTemplate =
    Remove 
        { getEvent = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "EVENT"
        , getStartID = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "ID"
        , getMax = 1
            &= Cmd.name "max"
            &= Cmd.typ "MAXIMUM" }
    &= Cmd.help "Removes a set of snapshots from an event"

restoreActionTemplate :: Action
restoreActionTemplate =
    Restore
        { getEvent = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "EVENT"
        , getID = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "ID" }
    &= Cmd.help "Restores a specific snapshot of an event"

actionMode :: Mode (CmdArgs Action)
actionMode = 
    Cmd.cmdArgsMode $
        Cmd.modes 
            [ testActionTemplate &= Cmd.auto
            , takeActionTemplate
            , recordActionTemplate
            , listActionTemplate
            , clearActionTemplate
            , removeActionTemplate
            , restoreActionTemplate ]
        &= Cmd.helpArg [Cmd.explicit, Cmd.name "help", Cmd.name "h"]
 
parseAction :: IO Action
parseAction = Cmd.cmdArgsRun actionMode

parse :: [String] -> IO Command
parse args = do
    let bridgeArgs = take 2 args
    let actionArgs = drop 2 args
    bridge <- (withArgs bridgeArgs parseBridge)
    action <- (withArgs actionArgs parseAction)
    return $ Command bridge action 
