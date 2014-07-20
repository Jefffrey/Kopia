module Kopia.Model.Snapshot
    ( Snapshot(..)
    , getLocalTime
    ) where

import Data.Time
    ( UTCTime
    , LocalTime
    , getCurrentTimeZone
    , utcToLocalTime )
import Kopia.Model.Bridge (Bridge)

data Snapshot
    = Snapshot
        { getEvent     :: String
        , getBridge    :: Bridge
        , getTime      :: UTCTime }
    deriving (Eq, Show)

getLocalTime :: Snapshot -> IO LocalTime
getLocalTime snapshot = do
    let time = getTime snapshot
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone time
