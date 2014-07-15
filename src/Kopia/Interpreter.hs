module Kopia.Interpreter (execute) where

import System.FilePath ((</>))
import Data.Time (UTCTime, formatTime, readTime, getCurrentTime)
import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import System.Locale (defaultTimeLocale)
import Kopia.Command
import Kopia.Filesystem (copyDir)
import qualified System.Directory as Dir

showMessage :: String -> IO ()
showMessage = putStrLn . ("\n"++)

showError :: String -> IO ()
showError = putStrLn . ("\n"++)

formatIOError :: IOError -> String
formatIOError e = 
    let f = ioeGetFileName e
        s = ioeGetErrorString e
    in case f of
        Nothing -> s
        Just x -> s ++ " (" ++ x ++ ")"

formatUTC :: UTCTime -> String
formatUTC utc = formatTime defaultTimeLocale "kopia_%d-%m-%y_%H-%M-%S" utc

readUTC :: String -> UTCTime
readUTC str = readTime defaultTimeLocale "kopia_%d-%m-%y_%H-%M-%S" str

execTake :: String -> Bridge -> IO ()
execTake name (Bridge target destination) = do
    catchIOError 
        (do
            t <- getCurrentTime
            let fullDestination = destination </> name </> formatUTC t
            copyDir target fullDestination 
            showMessage . unlines $
                [ "Snapshot taken"
                , "Time: " ++ show t
                , "Event name: " ++ name
                , "Target directory: " ++ target
                , "Destination directory: " ++ destination
                , "Snapshot location: " ++ fullDestination ])
        (\e -> do
            showError . unlines $
                [ "Couldn't take snapshot"
                , "Error: " ++ formatIOError e ])

execute :: Command -> IO ()
execute (Command b (Take n)) = execTake n b
execute _ = undefined
