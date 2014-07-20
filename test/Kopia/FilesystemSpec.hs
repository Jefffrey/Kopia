module Kopia.FilesystemSpec (spec) where

import Test.Hspec
import Control.Exception (bracket)
import System.FilePath ((</>))
import System.IO (writeFile)
import Control.Monad (when)
import Data.Time (getCurrentTime)
import System.Directory
import System.Exit (ExitCode(..))
import Kopia.Model.Bridge
import Kopia.Model.Order
import Kopia.Filesystem
import qualified Kopia.Model.Snapshot as Snapshot

dismantleSandbox :: FilePath -> IO ()
dismantleSandbox p = do
    e <- doesDirectoryExist p
    when e (removeDirectoryRecursive p)

setupSandbox :: IO FilePath
setupSandbox = do
    cd <- getCurrentDirectory 
    let sd = cd </> "test" </> "sandbox"
    dismantleSandbox sd 
    mapM_ createDirectory
        [ sd
        , sd </> "target"
        , sd </> "target" </> "a"
        , sd </> "target" </> "b"
        , sd </> "target" </> "b" </> "ba" ]
    mapM_ ((flip writeFile) "something")
        [ sd </> "target" </> "1.txt"
        , sd </> "target" </> "2.txt"
        , sd </> "target" </> "a" </> "3.txt"
        , sd </> "target" </> "a" </> "4.txt"
        , sd </> "target" </> "a" </> "5.txt" ]
    return $ sd

sandboxBridge :: FilePath -> Bridge
sandboxBridge p = Bridge (p </> "target") (p </> "destination")

withSandbox :: (Bridge -> IO ()) -> IO ()
withSandbox action =
    bracket
        setupSandbox
        dismantleSandbox
        (action . sandboxBridge)

spec :: Spec
spec = do
    describe "Interpreter" $ do
        describe "taking snapshots" $ do
            it "shouldn't throw exceptions while taking a snapshot" $ do
                withSandbox $ \b -> do
                    s <- takeSnapshot "event" b
                    Snapshot.getEvent s `shouldBe` "event"
                    Snapshot.getBridge s `shouldBe` b
            it "should have no problems with multiple takes" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event" b) [1..32]
            it "the time should be correct" $ do
                withSandbox $ \b -> do
                    before <- getCurrentTime
                    snapshot <- takeSnapshot "event" b
                    after <- getCurrentTime
                    let isBetween a b e = e >= a && e <= b
                    Snapshot.getTime snapshot 
                        `shouldSatisfy` (isBetween before after)
        describe "listing snapshots" $ do
            it "should give an empty list for non existent event" $ do
                withSandbox $ \b -> do
                    l <- listSnapshots "non_existent" 10 Newest b
                    l `shouldBe` []
            it "should return the list of snapshots for an event" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event" b) [1..14]
                    l <- listSnapshots "event" 20 Newest b
                    length l `shouldBe` 14
            it "shouldn't clash with different events" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event_a" b) [1..5]
                    mapM_ (const $ takeSnapshot "event_b" b) [1..9]
                    la <- listSnapshots "event_a" 20 Newest b
                    lb <- listSnapshots "event_b" 20 Newest b
                    length la `shouldBe` 5
                    length lb `shouldBe` 9
            it "should limit the results" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event" b) [1..24]
                    la <- listSnapshots "event" 30 Newest b
                    lb <- listSnapshots "event" 14 Newest b
                    lc <- listSnapshots "event" 1 Newest b
                    length la `shouldBe` 24
                    length lb `shouldBe` 14
                    length lc `shouldBe` 1
            it "should return all the snapshots for limit = 0" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event" b) [1..16]
                    l <- listSnapshots "event" 0 Newest b
                    length l `shouldBe` 16
        describe "clearing events" $ do
            it "should throw on missing event" $ do
                withSandbox $ \b -> do
                    clearEvent "missing" b `shouldThrow` anyIOException
            it "should clear an event" $ do
                withSandbox $ \b -> do
                    mapM_ (const $ takeSnapshot "event_a" b) [1..8]
                    mapM_ (const $ takeSnapshot "event_b" b) [1..4]

                    clearEvent "event_a" b
                    eventACount <- listSnapshots "event_a" 0 Oldest b
                    eventBCount <- listSnapshots "event_b" 0 Oldest b
                    length eventACount `shouldBe` 0
                    length eventBCount `shouldBe` 4

                    clearEvent "event_b" b
                    eventACount <- listSnapshots "event_a" 0 Oldest b
                    eventBCount <- listSnapshots "event_b" 0 Oldest b
                    length eventACount `shouldBe` 0
                    length eventBCount `shouldBe` 0
