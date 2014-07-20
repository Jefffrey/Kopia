module Kopia.ParserSpec where

import Test.Hspec
import Kopia.Parser
import Kopia.Model.Command
import Kopia.Model.Bridge
import System.Exit (ExitCode(..))
    
spec :: Spec
spec = do
    describe "Parser" $ do
        describe "Bridge" $ do
            it "should parse a bridge" $ do
                command <- parse ["abc", "def"]
                command `shouldBe` Command (Bridge "abc" "def") Test
            it "should throw on lack of a bridge" $ do
                parse ["acb"] `shouldThrow` (== ExitFailure 1)
                parse [] `shouldThrow` (== ExitFailure 1)
        describe "Action" $ do
            describe "test" $ do
                it "should parse a test command" $ do
                    command <- parse ["a", "b", "test"]
                    command `shouldBe` Command (Bridge "a" "b") Test
                it "should throw on too many arguments" $ do
                    parse ["a", "b", "test", "extra"]
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
            describe "list" $ do
                it "should parse a list command" $ do
                    a <- parse ["a", "b", "list", "name"]
                    a `shouldBe` Command (Bridge "a" "b") (List "name" 100 Newest)
                    b <- parse ["a", "b", "list", "name", "--max=123"]
                    b `shouldBe` Command (Bridge "a" "b") (List "name" 123 Newest)
                    c <- parse ["a", "b", "list", "name", "--order=oldest"]
                    c `shouldBe` Command (Bridge "a" "b") (List "name" 100 Oldest)
                it "should throw if MAXIMUM is not a number" $ do
                    parse ["a", "b", "list", "name", "--max=abc"]
                        `shouldThrow` (== ExitFailure 1)
                it "should throw if ORDER is not valid" $ do
                    parse ["a", "b", "list", "name", "--order=abc"]
                        `shouldThrow` (== ExitFailure 1)
            describe "clear" $ do
                it "should parse a clear command" $ do
                    a <- parse ["a", "b", "clear", "name"]
                    a `shouldBe` Command (Bridge "a" "b") (Clear "name")
                it "should throw on lack of arguments" $ do
                    parse ["a", "b", "clear"]
                        `shouldThrow` (== ExitFailure 1)
                it "should throw on too many arguments" $ do
                    parse ["a", "b", "clear", "name", "extra"]
                        `shouldThrow` (== ExitFailure 1)
            describe "remove" $ do
                it "should parse a remove command" $ do
                    a <- parse ["a", "b", "remove", "name", "123"]
                    a `shouldBe` Command (Bridge "a" "b") (Remove "name" 123 1)
                    b <- parse ["a", "b", "remove", "name", "123", "--max=456"]
                    b `shouldBe` Command (Bridge "a" "b") (Remove "name" 123 456)
                it "should enforce integral arguments" $ do
                    parse ["a", "b", "remove", "name", "abc"]
                        `shouldThrow` (== ExitFailure 1)
                    parse ["a", "b", "remove", "name", "123", "--max=abc"]
                        `shouldThrow` (== ExitFailure 1)
                it "should throw on lack of arguments" $ do
                    parse ["a", "b", "remove"]
                        `shouldThrow` (== ExitFailure 1)
                    parse ["a", "b", "remove", "name"]
                        `shouldThrow` (== ExitFailure 1)
            describe "restore" $ do
                it "should parse a restore command" $ do
                    a <- parse ["a", "b", "restore", "name", "123"]
                    a `shouldBe` Command (Bridge "a" "b") (Restore "name" 123)
                it "should enforce integral arguments" $ do
                    parse ["a", "b", "restore", "name", "abc"]
                        `shouldThrow` (== ExitFailure 1)
