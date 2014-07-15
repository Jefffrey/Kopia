module Kopia.Filesystem (copyDir) where

import System.FilePath ((</>))
import Control.Monad (when)
import qualified System.Directory as Dir

copyElem :: FilePath -> FilePath -> String -> IO ()
copyElem from to n = do
    let fromN = from </> n
    let toN = to </> n
    isFile <- Dir.doesFileExist fromN
    isDir <- Dir.doesDirectoryExist fromN
    Dir.createDirectoryIfMissing True to
    when isFile $ Dir.copyFile fromN toN
    when isDir $ copyDir fromN toN

copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
    contents <- Dir.getDirectoryContents from
    (mapM_ (copyElem from to) . filter (\i -> i /= "." && i /= "..")) contents
