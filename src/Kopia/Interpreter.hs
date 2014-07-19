module Kopia.Interpreter (execute) where

import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import Kopia.Command (Command(..), Action(..))
import Kopia.Bridge (Bridge(..))
import Kopia.Order (Order(..))
import Kopia.Filesystem (copyDir)
import System.Directory (getDirectoryContents)
import Data.Time (getCurrentTimeZone, utcToLocalTime, formatTime, LocalTime)
import System.Locale (defaultTimeLocale)
import Kopia.Snapshot (Snapshot)
import Control.Monad ((<=<))
import qualified Kopia.Snapshot as Snapshot

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
            putStrLn "Could perform action!"
            print error
            putStr "\n")

execTake :: String -> Bridge -> IO ()
execTake event bridge = renderSession $ do
    snapshot <- Snapshot.take event bridge
    putStrLn "Snapshot taken!\n"
    localTime <- Snapshot.getLocalTime snapshot
    putIndentLn $ "Time: " ++ showTime localTime

execList :: String -> Int -> Order -> Bridge -> IO ()
execList event maximum order bridge = 
    renderSession $ do
        snapshotsList <- Snapshot.list event maximum order bridge
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
