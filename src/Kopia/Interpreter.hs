module Kopia.Interpreter (execute) where

import System.FilePath ((</>))
import Data.Time (UTCTime)
import System.IO.Error (catchIOError, ioeGetErrorString, ioeGetFileName)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Kopia.Command
import qualified System.Directory as Dir
import qualified Data.Time as Time

kopiaName :: String -> UTCTime -> String
kopiaName name utc =
    let day = Time.utctDay utc
        time = floor . Time.utctDayTime $ utc :: Int
        (yyyy, mm, dd) = Time.toGregorian $ day
        hh = (time `div` 3600)
        mmm = (time `mod` 3600) `div` 60
        ss = (time `mod` 3600) `mod` 60
    in (name </>) . concat . intersperse "_" $
        [ show dd
        , show mm
        , show yyyy
        , show hh
        , show mmm
        , show ss ]

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

execTake :: String -> Bridge -> IO ()
execTake name (Bridge source target) = do
    catchIOError 
        (do
            t <- Time.getCurrentTime
            let kName = kopiaName name t
            copyDir target (source </> kName))
        (\_ -> putStrLn "Couldn't take snapshot")

execute :: Command -> IO ()
execute (Command b (Take n)) = execTake n b
execute _ = undefined
