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
            describe "take" $ do
                it "should parse a take command" $ do
                    command <- parse ["a", "b", "take", "name"]
                    command `shouldBe` Command (Bridge "a" "b") (Take "name")
                it "should throw on lack of arguments" $ do
                    parse ["a", "b", "take"] `shouldThrow` (== ExitFailure 1)
                it "should throw on too many arguments" $ do
                    parse ["a", "b", "name", "extra"]
                        `shouldThrow` (== ExitFailure 1)
            describe "record" $ do
                it "should parse a record command" $ do
                    a <- parse ["a", "b", "record", "name", "123"]
                    a `shouldBe` Command (Bridge "a" "b") (Record "name" 123 1)
                    b <- parse ["a", "b", "record", "name", "123", "--max=456"]
                    b `shouldBe` Command (Bridge "a" "b") (Record "name" 123 456)
                it "should throw on lack of arguments" $ do
                    parse ["a", "b", "record"] `shouldThrow` (== ExitFailure 1)                    
                    parse ["a", "b", "record", "name"]
                        `shouldThrow` (== ExitFailure 1)
                    parse ["a", "b", "record", "--max=456"]
                        `shouldThrow` (== ExitFailure 1)
                it "should throw on too many arguments" $ do
                    parse ["a", "b", "record", "123", "extra"] 
                        `shouldThrow` (== ExitFailure 1)
                it "should throw if MINUTES is not a number" $ do
                    parse ["a", "b", "record", "abc"]
                        `shouldThrow` (== ExitFailure 1)
                it "should throw if MAXIMUM is not a number" $ do
                    parse ["a", "b", "record", "123", "--max=abc"]
                        `shouldThrow` (== ExitFailure 1)
