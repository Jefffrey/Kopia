module Kopia.Interpreter (execute) where

import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import Kopia.Command (Command(..), Action(..))
import Kopia.Bridge (Bridge(..))
import Kopia.Order (Order(..))
import Kopia.Filesystem (copyDir)
import System.Directory (getDirectoryContents)
import qualified Kopia.Snapshot as Snapshot

renderSession :: IO () -> IO ()
renderSession a = do
    catchIOError 
        (do
            putStr "\n"
            a
            putStr "\n")
        (\e -> do
            putStr "\n"
            print e
            putStr "\n")

execTake :: String -> Bridge -> IO ()
execTake e b = 
    renderSession $ do
        s <- Snapshot.take e b
        putStr "Snapshot taken!\n\n"
        putStr . unlines . map ("    "++) . lines . show $ s

execList :: String -> Int -> Order -> Bridge -> IO ()
execList e m o b =
    renderSession $ do
        sl <- Snapshot.list e m o b
        putStr $ "Listing " ++ (show . length $ sl) ++ " snapshots:\n\n"
        mapM_ (\s -> do
            putStr . ("    "++) . (++") ") . show . fst $ s
            putStr . show . Snapshot.localTime . snd $ s
            putStr "\n") sl

execute :: Command -> IO ()
execute (Command b (Take e)) = execTake e b
execute (Command b (List e m o)) = execList e m o b
execute _ = undefined
