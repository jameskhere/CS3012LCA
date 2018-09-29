module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Divide Function" $ do

    it "returns Just 0 for 0/x" $ do
      (divideSafe 0 3) == Just 0  `shouldBe` True

    it "returns Just 5 for 10/2" $ do
      (divideSafe 10 2) `shouldBe` Just 5

    it "returns Nothing for x/0" $ do
      (divideSafe 10 0) `shouldBe` Nothing

    -- describe "should return nothing for x > 100" $ do
    it "should return nothing for x > 100, y == 0" $ do
      (divideSafe 101 0) `shouldBe` Nothing

    it "should return nothing for x > 100, y == 10" $ do
      (divideSafe 101 10) `shouldBe` Nothing

