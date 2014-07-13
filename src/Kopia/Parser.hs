module Kopia.Parser (parse) where

import Prelude hiding (max, id)
import System.Environment (withArgs)
import System.Console.CmdArgs ((&=), CmdArgs, Mode, Default)
import System.Console.CmdArgs.Explicit (process)
import Kopia.Command (Command(..), Bridge(..), Action(..))
import qualified System.Console.CmdArgs as Cmd

bridgeTemplate :: Bridge
bridgeTemplate = 
    Bridge 
        { storage = Cmd.def 
            &= Cmd.argPos 0 
            &= Cmd.typ "STORAGE"
        , target = Cmd.def 
            &= Cmd.argPos 1 
            &= Cmd.typ "TARGET" }
    &= Cmd.summary "Kopia - backup system"
    &= Cmd.help "Estabilish bridge"
    &= Cmd.helpArg [Cmd.explicit, Cmd.name "help", Cmd.name "h"]
    &= Cmd.program "kopia"

parseBridge :: IO Bridge
parseBridge = Cmd.cmdArgsRun $ Cmd.cmdArgsMode bridgeTemplate

statusActionTemplate :: Action
statusActionTemplate = Status &= Cmd.help "Check status of the bridge"

takeActionTemplate :: Action
takeActionTemplate = 
    Take 
        { name = Cmd.def 
            &= Cmd.argPos 0 
            &= Cmd.typ "NAME" }
    &= Cmd.help "Takes a snapshot for an event"

recordActionTemplate :: Action
recordActionTemplate =
    Record
        { name = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "NAME"
        , minutes = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "MINUTES"
        , max = 1
            &= Cmd.typ "MAXIMUM" }
    &= Cmd.help "Takes a serie of snapshots every MINUTES minutes"
                
listActionTemplate :: Action
listActionTemplate =
    List
        { name = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "NAME"
        , max = 100
            &= Cmd.typ "MAXIMUM" }
    &= Cmd.help "Lists all the snapshots of an event"

clearActionTemplate :: Action
clearActionTemplate =
    Clear
        { name = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "NAME" }
    &= Cmd.help "Deletes every snapshot of an event"

removeActionTemplate :: Action
removeActionTemplate =
    Remove 
        { name = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "NAME"
        , from = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "ID"
        , max = 1
            &= Cmd.typ "MAXIMUM" }
    &= Cmd.help "Removes a set of snapshots from an event"

restoreActionTemplate :: Action
restoreActionTemplate =
    Restore
        { name = Cmd.def
            &= Cmd.argPos 0
            &= Cmd.typ "NAME"
        , id = Cmd.def
            &= Cmd.argPos 1
            &= Cmd.typ "ID" }
    &= Cmd.help "Restores a specific snapshot of an event"

actionMode :: Mode (CmdArgs Action)
actionMode = 
    Cmd.cmdArgsMode $
        Cmd.modes 
            [ statusActionTemplate &= Cmd.auto
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
