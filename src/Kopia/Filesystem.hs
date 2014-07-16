module Kopia.Filesystem
    ( copyDir
    , listDirs
    ) where

import System.FilePath ((</>))
import Control.Monad (when, filterM)
import qualified System.Directory as Dir

listAll :: FilePath -> IO [String]
listAll p = do
    c <- Dir.getDirectoryContents p
    let r = filter (\i -> i /= "." && i /= "..") c
    return r

listDirs :: FilePath -> IO [String]
listDirs p = do
    c <- listAll p
    filterM (Dir.doesDirectoryExist . (p </>)) c

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
    contents <- listAll from
    mapM_ (copyElem from to) contents
