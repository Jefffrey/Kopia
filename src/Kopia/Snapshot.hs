module Kopia.Snapshot
    ( Snapshot
    , getTime
    , getLocalTime
    , getEvent
    , getBridge
    , getLocation
    , take
    , list
    ) where

import Data.Time
    ( UTCTime
    , LocalTime
    , formatTime
    , parseTime
    , getCurrentTime
    , getCurrentTimeZone
    , utcToLocalTime )
import Kopia.Bridge (Bridge)
import Kopia.Order (Order(..))
import Kopia.Filesystem (copyDir, listDirs)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Function (on)
import System.Directory (doesDirectoryExist)
import Control.Monad (foldM)
import Prelude hiding (take)
import qualified Kopia.Bridge as Bridge
import qualified Prelude as Prelude

data Snapshot
    = Snapshot
        { getTime      :: UTCTime
        , getEvent     :: String
        , getBridge    :: Bridge }
    deriving (Eq, Show)

formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q"

readUTC :: String -> Maybe UTCTime
readUTC = parseTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q"

getLocation :: Snapshot -> FilePath
getLocation snapshot =   (Bridge.destination . getBridge $ snapshot) 
                  </> (getEvent snapshot) 
                  </> (formatUTC . getTime $ snapshot)

getLocalTime :: Snapshot -> IO LocalTime
getLocalTime snapshot = do
    let time = getTime snapshot
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone time

listSnapshots :: String -> Bridge -> IO [Snapshot]
listSnapshots event bridge = do
    let eventPath = Bridge.destination bridge </> event
    eventDirExist <- doesDirectoryExist eventPath
    if eventDirExist
        then do
            directories <- listDirs eventPath
            let fn snapshotList snapshotName = do
                    let maybeTime = readUTC snapshotName
                    case maybeTime of
                        Just time -> return $ 
                            Snapshot time event bridge
                                : snapshotList
                        Nothing -> return snapshotList
            foldM fn [] directories
        else return []

reduceSnapshots :: Int -> [Snapshot] -> [Snapshot]
reduceSnapshots maximum =
    if maximum > 0
        then Prelude.take maximum
        else id

sortSnapshots :: Order -> [Snapshot] -> [Snapshot]
sortSnapshots order =
    let comparator = 
            case order of
                Oldest -> compare
                Newest -> flip compare
    in  sortBy (comparator `on` getTime)

take :: String -> Bridge -> IO Snapshot
take event bridge = do
    time <- getCurrentTime
    let snapshot = Snapshot time event bridge
    copyDir (Bridge.target bridge) (getLocation snapshot)
    return snapshot

list :: String -> Int -> Order -> Bridge -> IO [Snapshot]
list event maximum order bridge = do
    snapshots <- listSnapshots event bridge
    return (sortSnapshots order . reduceSnapshots maximum $ snapshots)
