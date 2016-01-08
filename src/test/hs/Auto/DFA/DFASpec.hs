module Auto.DFA.DFASpec where

import Auto.DFA.DFA
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "newDFA" $ do
        prop "has no states" $
            states newDFA `shouldBe` []

        prop "has no states" $
            states newDFA `shouldBe` []

        prop "has no initial state" $
            initial newDFA `shouldBe` Nothing

        prop "has no accepting states" $
            accepting newDFA `shouldBe` []

    describe "addState" $ do
        prop "contains state after addition" $ \s ->
            states (addState s newDFA) `shouldBe` [s]

    describe "addState" $ do
        prop "contains two states after addition" $ \(s1, s2) ->
            s1 /= s2 ==> s1 `elem` (states (addState s1 $ addState s2 newDFA)) `shouldBe` True

    describe "setInitial" $ do
        prop "should set initial state" $ \s ->
            initial (setInitial s $ addState s newDFA) `shouldBe` (Just $ s)


