module Kopia.Filesystem
    ( takeSnapshot
    , listSnapshots
    , clearEvent
    ) where

import Data.Time (UTCTime, getCurrentTime, formatTime, parseTime)
import System.FilePath ((</>))
import Control.Monad (when, filterM)
import System.Directory
    ( getDirectoryContents
    , doesDirectoryExist
    , doesFileExist
    , createDirectoryIfMissing
    , copyFile
    , removeDirectoryRecursive )
import System.Locale (defaultTimeLocale)
import Data.List (sortBy)
import Data.Function (on)
import Kopia.Model.Bridge (Bridge)
import Kopia.Model.Order (Order(..))
import Kopia.Model.Snapshot (Snapshot(..))
import qualified Kopia.Model.Bridge as Bridge
import qualified Kopia.Model.Snapshot as Snapshot

formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q"

readUTC :: String -> Maybe UTCTime
readUTC = parseTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q"

listAll :: FilePath -> IO [String]
listAll p = do
    c <- getDirectoryContents p
    let r = filter (\i -> i /= "." && i /= "..") c
    return r

listDirs :: FilePath -> IO [String]
listDirs p = do
    c <- listAll p
    filterM (doesDirectoryExist . (p </>)) c

copyElem :: FilePath -> FilePath -> String -> IO ()
copyElem from to n = do
    let fromN = from </> n
    let toN = to </> n
    isFile <- doesFileExist fromN
    isDir <- doesDirectoryExist fromN
    createDirectoryIfMissing True to
    when isFile $ copyFile fromN toN
    when isDir $ copyDir fromN toN

copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
    contents <- listAll from
    mapM_ (copyElem from to) contents

getSnapshotLocation :: Snapshot -> FilePath
getSnapshotLocation snapshot =
        (Bridge.getDestination . Snapshot.getBridge $ snapshot) 
    </> (Snapshot.getEvent snapshot) 
    </> (formatUTC . Snapshot.getTime $ snapshot)

getEventLocation :: String -> Bridge -> FilePath
getEventLocation event bridge =
    (Bridge.getDestination bridge) </> event

listAllSnapshots :: String -> Bridge -> IO [Snapshot]
listAllSnapshots event bridge = do
    let eventPath = Bridge.getDestination bridge </> event
    eventDirExist <- doesDirectoryExist eventPath
    if eventDirExist
        then do
            directories <- listDirs eventPath
            let fn snapshotName snapshotList = do
                    let maybeTime = readUTC snapshotName
                    case maybeTime of
                        Just time ->
                            Snapshot event bridge time
                                : snapshotList
                        Nothing -> snapshotList
            return $ foldr fn [] directories
        else return []

reduceSnapshots :: Int -> [Snapshot] -> [Snapshot]
reduceSnapshots maximum =
    if maximum > 0
        then take maximum
        else id

sortSnapshots :: Order -> [Snapshot] -> [Snapshot]
sortSnapshots order =
    let comparator = 
            case order of
                Oldest -> compare
                Newest -> flip compare
    in  sortBy (comparator `on` getTime)

takeSnapshot :: String -> Bridge -> IO Snapshot
takeSnapshot event bridge = do
    time <- getCurrentTime
    let snapshot = Snapshot event bridge time
    copyDir (Bridge.getTarget bridge) (getSnapshotLocation snapshot)
    return snapshot

listSnapshots :: String -> Int -> Order -> Bridge -> IO [Snapshot]
listSnapshots event maximum order bridge = do
    snapshots <- listAllSnapshots event bridge
    return (sortSnapshots order . reduceSnapshots maximum $ snapshots)

clearEvent :: String -> Bridge -> IO ()
clearEvent event bridge = do
    let eventPath = getEventLocation event bridge
    removeDirectoryRecursive eventPath
