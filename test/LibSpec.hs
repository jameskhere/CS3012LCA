module LibSpec (spec, bstContains, bstLCA, BST, bstEmpty, bstRoot, bstChar, bstSample) where

import          Lib
import          Test.Hspec
import          Test.QuickCheck

spec :: Spec
spec = do
    describe "LCA" $ do
--          this is bstChar
--                 'm'
--                /  \
--             'c'   'q'
--             /       \
--          'a'        't'
--
--          this is bstSample
--                 7
--            /        \
--           2         12
--          / \        / \
--         1   5      9  13
--            / \    / \   \
--           4   6  8  11  15
--          /          /   / \
--         3          10  14 16


        it "returns False for emptyBST contains 2" $ do
            (bstContains bstEmpty 2) `shouldBe` False

        it "returns True for bstRoot contains 10" $ do
            (bstContains bstRoot 10) `shouldBe` True
        it "returns False for bstRoot contains 253" $ do
            (bstContains bstRoot 253) `shouldBe` False

        it "returns True for bstChar contains 'm'" $ do
            (bstContains bstChar 'm') `shouldBe` True
        it "returns False for bstChar contains 'X'" $ do
            (bstContains bstChar 'X') `shouldBe` False

        it "returns True for bstSample contains 1" $ do
            (bstContains bstSample 1) `shouldBe` True
        it "returns True for bstSample contains 7" $ do
            (bstContains bstSample 7) `shouldBe` True
        it "returns False for bstSample contains 20" $ do
            (bstContains bstSample 20) `shouldBe` False

        it "using bstEmpty, returns Nothing for 2 and 17" $ do
            (bstLCA bstEmpty 2 17) `shouldBe` 9

        it "using bstSample, returns 9 as LCA for 8 and 10" $ do
            (bstLCA bstSample 8 10) `shouldBe` 9
