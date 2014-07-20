module Kopia.Interpreter (execute) where

import System.IO.Error (catchIOError)
import Data.Time (getCurrentTimeZone, utcToLocalTime, formatTime, LocalTime)
import System.Locale (defaultTimeLocale)
import Control.Monad ((<=<))
import Kopia.Filesystem
import Kopia.Model.Order (Order(..))
import Kopia.Model.Command (Command(..), Action(..))
import Kopia.Model.Snapshot (Snapshot)
import Kopia.Model.Bridge (Bridge)
import qualified Kopia.Model.Bridge as Bridge
import qualified Kopia.Model.Snapshot as Snapshot

indent :: String -> String
indent = ("    " ++)

putIndentLn :: String -> IO ()
putIndentLn = putStrLn . indent

showTime :: LocalTime -> String
showTime = formatTime defaultTimeLocale "%d %B %Y (%A) at %H:%M:%S:%q"

renderSession :: IO () -> IO ()
renderSession action = do
    catchIOError 
        (do
            putStr "\n"
            action
            putStr "\n")
        (\error -> do
            putStr "\n"
            putStrLn "Couldn't perform action!"
            print error
            putStr "\n")

execTake :: String -> Bridge -> IO ()
execTake event bridge = renderSession $ do
    snapshot <- takeSnapshot event bridge
    putStrLn "Snapshot taken!\n"
    localTime <- Snapshot.getLocalTime snapshot
    putIndentLn $ "Time: " ++ showTime localTime

execList :: String -> Int -> Order -> Bridge -> IO ()
execList event maximum order bridge = 
    renderSession $ do
        snapshotsList <- listSnapshots event maximum order bridge
        let snapshotsCount = length snapshotsList
        if snapshotsCount > 0
            then putStrLn $ 
                "Listing " ++ (show snapshotsCount) ++ " snapshot(s):\n"
            else putStrLn "No snapshots"
        mapM_ (putIndentLn . showTime <=< Snapshot.getLocalTime) snapshotsList

execute :: Command -> IO ()
execute (Command bridge (Take event)) = 
    execTake event bridge
execute (Command bridge (List event maximum order)) = 
    execList event maximum order bridge
execute _ = undefined
