module Kopia.Interpreter (execute) where

import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import Kopia.Command (Command(..), Action(..))
import Kopia.Bridge (Bridge(..))
import Kopia.Order (Order(..))
import Kopia.Filesystem (copyDir)
import System.Directory (getDirectoryContents)
import qualified Kopia.Snapshot as Snapshot

indent :: String -> String
indent = ("    " ++)

putIndentLn :: String -> IO ()
putIndentLn = putStr . indent

renderSession :: String -> IO () -> IO ()
renderSession s a = do
    catchIOError 
        (do
            putStr "\n"
            a
            putStr "\n")
        (\e -> do
            putStr "\n"
            putStrLn s
            print e
            putStr "\n")

execTake :: String -> Bridge -> IO ()
execTake e b = renderSession "Could take snapshot" $ do
    s <- Snapshot.take e b
    putStrLn "Snapshot taken!\n"
    putStr . unlines . map indent . lines . show $ s

execList :: String -> Int -> Order -> Bridge -> IO ()
execList e m o b = 
    renderSession "Couldn't retrieve snapshots" $ do
        sl <- Snapshot.list e m o b
        let l = length sl
        if l > 0
            then putStrLn $ "Listing " ++ (show l) ++ " snapshots:\n"
            else putStrLn "No snapshots"
        mapM_ (\s -> do
            putIndentLn . (++") ") . show . fst $ s
            putStr . show . Snapshot.localTime . snd $ s
            putStr "\n") sl

execute :: Command -> IO ()
execute (Command b (Take e)) = execTake e b
execute (Command b (List e m o)) = execList e m o b
execute _ = undefined
