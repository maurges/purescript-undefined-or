module Test.Main (main) where

import Prelude
import Data.Maybe (Maybe (Just, Nothing))
import Control.Alt ((<|>))
import Control.Plus (empty)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.UndefinedOr
    (UndefinedOr, isUndefined, fromUndefined, toUndefined, runUndefined)

foreign import dict :: {yes :: UndefinedOr String, no :: UndefinedOr Int}


main :: Effect Unit
main = launchAff_ <<< runSpec [consoleReporter] $ do
    describe "undefined-or" do
        it "Preserves wrapping" do
            let val = 5
            let u = toUndefined val
            isUndefined u `shouldEqual` false
            case fromUndefined u of
                Just val' -> val' `shouldEqual` val
                Nothing -> fail "Unwrapped to nothing"
        --
        describe "isUndefined" do
            it "Works on present foreign" do
                isUndefined dict.yes `shouldEqual` false
            it "Works on missing foreign" do
                isUndefined dict.no `shouldEqual` true
        --
        describe "fromUndefined" do
            it "Works on present foreign" do
                case fromUndefined dict.yes of
                    Just x -> x `shouldEqual` "yes"
                    Nothing -> fail "Got nothing from 'yes'"
            it "Works on missing foreign" do
                case fromUndefined dict.no of
                    Just x -> fail "Got something from 'no'"
                    Nothing -> pure unit
        --
        describe "runUndefined" do
            it "Works on present foreign" do
                runUndefined "no" identity dict.yes `shouldEqual` "yes"
            it "Works on missing foreign" do
                runUndefined "no" (const "wat") dict.no `shouldEqual` "no"
        describe "Eq UndefinedOr" do
            it "Compares present" do
                dict.yes `shouldEqual` toUndefined "yes"
                dict.yes `shouldNotEqual` toUndefined "no"
            it "Compares missing" do
                dict.no `shouldEqual` empty
            it "Compares different" do
                dict.yes `shouldNotEqual` empty
                dict.no `shouldNotEqual` toUndefined 10
        --
        describe "Functor UndefinedOr" do
            it "Maps present foreign" do
                map (_ <> "!") dict.yes `shouldEqual` toUndefined "yes!"
            it "Maps missing foreign" do
                isUndefined (map (_ + 5) dict.no) `shouldEqual` true
        --
        describe "Apply UndefinedOr" do
            it "Applies present to present" do
                (toUndefined (_ <> "!") <*> dict.yes) `shouldEqual` toUndefined "yes!"
            it "Applies present to missing" do
                isUndefined (toUndefined (_ + 5) <*> dict.no) `shouldEqual` true
            it "Applies missing" do
                isUndefined (empty <*> dict.yes) `shouldEqual` true
                isUndefined (empty <*> dict.no) `shouldEqual` true
        --
        describe "Applicative UndefinedOr" do
            it "Creates present" do
                isUndefined (pure 5) `shouldEqual` false
        --
        describe "Alt UndefinedOr" do
            it "Chooses present" do
                (dict.yes <|> empty) `shouldEqual` dict.yes
                (dict.no <|> toUndefined 10) `shouldEqual` toUndefined 10
        --
        describe "Plus UndefinedOr" do
            it "Creates undefined" do
                isUndefined empty `shouldEqual` true
