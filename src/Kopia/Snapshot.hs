module Kopia.Snapshot where

import Data.Time
    ( UTCTime
    , LocalTime
    , formatTime
    , readTime
    , utcToLocalTime
    , getCurrentTime
    , getCurrentTimeZone )
import Kopia.Bridge (Bridge)
import Kopia.Filesystem (copyDir)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import Data.List (intercalate)
import qualified Kopia.Bridge as Bridge

data Snapshot
    = Snapshot
        { time      :: UTCTime
        , localTime :: LocalTime
        , event     :: String
        , bridge    :: Bridge }
    deriving (Eq)

formatUTC :: UTCTime -> String
formatUTC utc = formatTime defaultTimeLocale "kopia_%d-%m-%y_%H-%M-%S" utc

readUTC :: String -> UTCTime
readUTC str = readTime defaultTimeLocale "kopia_%d-%m-%y_%H-%M-%S" str

formatLocalTime :: LocalTime -> String
formatLocalTime lt = formatTime defaultTimeLocale "%d-%m-%y %H:%M:%S" lt

location :: Snapshot -> FilePath
location s =
    (Bridge.destination . bridge $ s) </> (event s) </> (formatUTC . time $ s)

instance Show Snapshot where
    show s = 
        intercalate "\n" $
            [ "Time: " ++ (formatLocalTime . localTime $ s)
            , "Event: " ++ (event s)
            , "Target: " ++ (Bridge.target . bridge $ s)
            , "Destination: " ++ (Bridge.destination . bridge $ s)
            , "Location: " ++ (location s) ]

take :: String -> Bridge -> IO Snapshot
take e b = do
    t <- getCurrentTime
    tz <- getCurrentTimeZone
    let lt = utcToLocalTime tz t
    let s = Snapshot t lt e b
    copyDir (Bridge.target b) (location s)
    return s
