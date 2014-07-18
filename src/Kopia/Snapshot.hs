module Kopia.Snapshot
    ( Snapshot
    , localTime
    , formatLocalTime
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
    , parseTime
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
import System.Directory (doesDirectoryExist)
import Control.Monad (foldM)
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
formatUTC utc = formatTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q" utc

readUTC :: String -> Maybe UTCTime
readUTC str = parseTime defaultTimeLocale "%d-%m-%y_%H-%M-%S-%q" str

formatLocalTime :: LocalTime -> String
formatLocalTime lt = formatTime defaultTimeLocale "%d %B %Y (%A) at %H:%M:%S:%q" lt

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

list :: String -> Int -> Order -> Bridge -> IO [Snapshot]
list e m o b = do
    let ep = Bridge.destination b </> e
    ed <- doesDirectoryExist ep
    if ed 
        then do
            ds <- listDirs ep
            let fn cl p = do
                    let t = readUTC p
                    case t of
                        Just tf -> do
                            lt <- toLocalTime tf
                            return $ Snapshot tf lt e b : cl
                        Nothing -> return cl
            l <- foldM fn [] ds
            let r = sortBy (compare `on` time) l
            let ri = 
                    if m > 0
                        then Prelude.take m r
                        else r
            if o == Oldest
                then return $ ri
                else return $ reverse ri
        else return []
