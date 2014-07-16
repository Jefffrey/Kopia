module Kopia.Snapshot
    ( Snapshot
    , localTime
    , event
    , bridge
    , location
    , take
    , list
    ) where

import Data.Time
    ( UTCTime
    , LocalTime
    , formatTime
    , readTime
    , utcToLocalTime
    , getCurrentTime
    , getCurrentTimeZone )
import Kopia.Bridge (Bridge)
import Kopia.Order (Order(..))
import Kopia.Filesystem (copyDir, listDirs)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Function (on)
import Prelude hiding (take)
import qualified Kopia.Bridge as Bridge
import qualified Prelude as Prelude

data Snapshot
    = Snapshot
        { time      :: UTCTime
        , localTime :: LocalTime
        , event     :: String
        , bridge    :: Bridge }
    deriving (Eq)

formatUTC :: UTCTime -> String
formatUTC utc = formatTime defaultTimeLocale "%d-%m-%y_%H-%M-%S" utc

readUTC :: String -> UTCTime
readUTC str = readTime defaultTimeLocale "%d-%m-%y_%H-%M-%S" str

formatLocalTime :: LocalTime -> String
formatLocalTime lt = formatTime defaultTimeLocale "%d-%m-%y %H:%M:%S" lt

toLocalTime :: UTCTime -> IO LocalTime
toLocalTime t = do
    tz <- getCurrentTimeZone
    return $ utcToLocalTime tz t

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
    lt <- toLocalTime t 
    let s = Snapshot t lt e b
    copyDir (Bridge.target b) (location s)
    return s

assignIDs :: Int -> [Snapshot] -> [(Int, Snapshot)]
assignIDs _ [] = []
assignIDs i (x:xs) = (i, x) : assignIDs (i + 1) xs

list :: String -> Int -> Order -> Bridge -> IO [(Int, Snapshot)]
list e m o b = do
    let ep = Bridge.destination b </> e
    ds <- listDirs ep
    
    let fn p = do
        let t = readUTC p
        lt <- toLocalTime t 
        return $ Snapshot t lt e b 
    l <- mapM fn ds
    let r = sortBy (compare `on` time) l
    let ri = Prelude.take m $ assignIDs 1 r
    if o == Oldest
        then return $ ri
        else return $ reverse ri
