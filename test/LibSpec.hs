module LibSpec (spec, bstContains, bstLCA, BST, bstEmpty, bstRoot, bstChar, bstSample) where

import          Lib
import          Test.Hspec
import          Test.QuickCheck

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    --dagContains on dagEmpty
    it "returns False for emptyDAG contains 2 (val not in graph)" $ do
      (dagContains dagEmpty 2) `shouldBe` False

    --dagContains on dagRoot
    it "returns True for dagRoot contains 10" $ do
      (dagContains dagRoot 10) `shouldBe` True
    it "returns False for dagRoot contains 20 (val not in graph)" $ do
      (dagContains dagRoot 20) `shouldBe` False

    --dagContains on dagSimple
    it "returns True for dagSimple contains 1" $ do
      (dagContains dagSimple 1) `shouldBe` True
    it "returns True for dagSimple contains 7" $ do
      (dagContains dagSimple 7) `shouldBe` True
    it "returns False for dagSimple contains 20 (val not in graph)" $ do
      (dagContains dagSimple 20) `shouldBe` False

    --dagContains on dagConnected
    it "returns True for dagConnected contains 1" $ do
      (dagContains dagConnected 1) `shouldBe` True
    it "returns True for dagConnected contains 7" $ do
      (dagContains dagConnected 7) `shouldBe` True
    it "returns True for dagConnected contains 20 (val not in graph)" $ do
      (dagContains dagConnected 20) `shouldBe` True

    --dagLCA on dagEmpty
    it "returns Nothing as LCA of 2 and 17 in dagEmpty (arbitrary values)" $ do
      (dagLCA dagEmpty 2 17) == Nothing `shouldBe` True

    --bstLCA on bstRoot
    it "returns Nothing as LCA of 4 and 5 in dagRoot (arbitrary values)" $ do
      (dagLCA dagRoot 4 5) == Nothing `shouldBe` True
    it "using dagRoot, returns root (10) as LCA of 10 and 10" $ do
      (dagLCA dagRoot 10 10) == Just 10 `shouldBe` True

    --dagLCA on dagSimple
    it "returns Nothing as LCA for 101 and 420 in dagSimple (arbitrary values)" $ do
      (dagLCA dagSimple 101 420) == Nothing `shouldBe` True
    it "using dagSimple, returns 9 as LCA for 8 and 10" $ do
      (dagLCA dagSimple 8 10) == Just 9 `shouldBe` True
    it "returns Nothing as LCA for 8 and 1000 in dagSimple (arbitrary values)" $ do
      (dagLCA dagSimple 8 1000) == Nothing `shouldBe` True

    --dagLCA on dagConnected
    it "returns Nothing as LCA for 101 and 420 in dagConnected (arbitrary values)" $ do
      (dagLCA dagSimple 101 420) == Nothing `shouldBe` True
    it "using dagConnected, returns 9 as LCA for 8 and 10" $ do
      (dagLCA dagSimple 8 10) == Just 9 `shouldBe` True
    it "returns Nothing as LCA for 8 and 1000 in dagConnected (arbitrary values)" $ do
      (dagLCA dagSimple 8 1000) == Nothing `shouldBe` True
