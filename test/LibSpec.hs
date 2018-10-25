module LibSpec (spec, Path, empty, cons, dagContains, dagLCA) where

import          Lib
import          Test.Hspec
import          Test.QuickCheck

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

dagEmpty = Path [] :# 0

dagRoot = Path [1] :# 1
--    dagRoot
--       1

dagSimple1 = Path [4, 2, 1] :# 3
dagSimple2 = Path [5, 3, 1] :# 3
dagSimple3 = Path [6, 3, 1] :# 3
--    dagSimple
--        1
--      /  \
--     2    3
--   /     / \
--  4     5  6

dagConnected1 = Path [4, 2, 1] :# 3
dagConnected2 = Path [9, 7, 5, 2, 1] :# 5
dagConnected3 = Path [9, 7, 5, 3, 1] :# 5
dagConnected4 = Path [9, 7, 6, 3, 1] :# 5
dagConnected5 = Path [8, 6, 3, 1] :# 4
--    dagConnected
--          1
--        /  \
--       2    3
--     /   \ / \
--    4     5   6
--          \  / \
--           7    8
--           \
--            9

    --dagContains on dagEmpty
    it "returns False for emptyDAG contains 2 (val not in graph)" $ do
      (dagContains dagEmpty 2) `shouldBe` False

    --dagContains on dagRoot
    it "returns True for dagRoot contains 10" $ do
      (dagContains dagRoot 1) `shouldBe` True
    it "returns False for dagRoot contains 20 (val not in graph)" $ do
      (dagContains dagRoot 20) `shouldBe` False

    --dagContains on dagSimple
    it "returns True for dagSimple (path 1) contains 1" $ do
      (dagContains dagSimple1 1) `shouldBe` True
    it "returns True for dagSimple (path 2) contains 5" $ do
      (dagContains dagSimple2 5) `shouldBe` True
    it "returns False for dagSimple (path 3) contains 5" $ do
      (dagContains dagSimple3 5) `shouldBe` False

    --dagContains on dagConnected
    it "returns True for dagConnected (path 1) contains 1" $ do
      (dagContains dagConnected1 1) `shouldBe` True
    it "returns True for dagConnected (path 2) contains 7" $ do
      (dagContains dagConnected2 7) `shouldBe` True
    it "returns True for dagConnected (path 4) contains 7" $ do
      (dagContains dagConnected4 7) `shouldBe` True

    --dagLCA on dagEmpty
    it "returns 0 as LCA of empty graphs" $ do
      (dagLCA emptyDAG emptyDAG) == 0 `shouldBe` True

    --bstLCA on bstRoot
    it "using dagRoot, returns root (1) as LCA of 1 and 1" $ do
      (dagLCA dagRoot dagRoot) == 1 `shouldBe` True

    --dagLCA on dagSimple
    it "returns 1 as LCA for path 1 & 2 in dagSimple" $ do
      (dagLCA dagSimple1 dagSimple2) == 1 `shouldBe` True
    it "returns 3 as LCA for path 2 & 3 in dagSimple" $ do
      (dagLCA dagSimple2 dagSimple3) == 3 `shouldBe` True

    --dagLCA on dagConnected
    it "returns 1 as LCA for path 1 & 5 in dagConnected" $ do
      (dagLCA dagconnected1 dagConnected5) == 1 `shouldBe` True
    it "returns 2 as LCA for path 1 & 2 in dagConnected" $ do
      (dagLCA dagConnected1 dagConnected2) == 2 `shouldBe` True
    it "returns 9 as LCA for path 3 & 4 in dagConnected" $ do
      (dagLCA dagConnected3 dagConnected4) == 9 `shouldBe` True
    it "returns 3 as LCA for path 3 & 5 in dagConnected" $ do
      (dagLCA dagConnected3 dagConnected5) == 3 `shouldBe` True
