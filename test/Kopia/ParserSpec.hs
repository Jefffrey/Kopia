module Kopia.ParserSpec where

import Test.Hspec
import Kopia.Parser
import Kopia.Command
import System.Exit (ExitCode(..))
    
spec :: Spec
spec = do
    describe "Parser" $ do
        describe "Bridge" $ do
            it "should parse a bridge" $ do
                command <- parse ["abc", "def"]
                command `shouldBe` Command (Bridge "abc" "def") Status
            it "should throw on lack of a bridge" $ do
                parse ["acb"] `shouldThrow` (== ExitFailure 1)
                parse [] `shouldThrow` (== ExitFailure 1)
        describe "Action" $ do
            describe "status" $ do
                it "should parse a status command" $ do
                    command <- parse ["a", "b", "status"]
                    command `shouldBe` Command (Bridge "a" "b") Status
                it "should throw on too many arguments" $ do
                    parse ["a", "b", "status", "extra"]
                        `shouldThrow` (== ExitFailure 1)
