module Kopia.Interpreter (execute) where

import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import Kopia.Command (Command(..), Action(..))
import Kopia.Bridge (Bridge(..))
import Kopia.Filesystem (copyDir)
import System.Directory (getDirectoryContents)
import qualified Kopia.Snapshot as Snapshot

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

execTake :: String -> Bridge -> IO ()
execTake e b = do
    catchIOError 
        (do
            s <- Snapshot.take e b
            showMessage . unlines $ ["Snapshot taken!", show s])
        (\e -> do
            showError . unlines $
                [ "Couldn't take snapshot"
                , "Error: " ++ formatIOError e ])

execute :: Command -> IO ()
execute (Command b (Take n)) = execTake n b
execute _ = undefined
