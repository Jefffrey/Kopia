module Kopia.InterpreterSpec (spec) where

import Test.Hspec
import Control.Exception (bracket)
import System.FilePath ((</>))
import System.IO (writeFile)
import Control.Monad (when)
import System.Directory
import Kopia.Bridge
import qualified Kopia.Snapshot as Snapshot

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
        describe "listing snapshots" $ do
            it "should give an empty list for non existent event" $ do
                withSandbox $ \b -> do
                    l <- Snapshot.list "non_existent" 10 Newest b
                    l `shouldBe` []
