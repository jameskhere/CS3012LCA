module LibSpec (spec, bstContains, bstLCA, BST, bstEmpty, bstRoot, bstChar, bstSample) where

import          Lib
import          Test.Hspec
import          Test.QuickCheck

spec :: Spec
spec = do
    describe "LCA" $ do
--      this is bstChar provided in Lib.hs
--                 'm'
--                /  \
--             'c'   'q'
--             /       \
--          'a'        't'
--
--      this is bstSample provided in Lib.hs
--                 7
--            /        \
--           2         12
--          / \        / \
--         1   5      9  13
--            / \    / \   \
--           4   6  8  11  15
--          /          /   / \
--         3          10  14 16

        --bstContains on bstEmpty
        it "returns False for emptyBST contains 2 (arbitrary value)" $ do
            (bstContains bstEmpty 2) `shouldBe` False

        --bstContains on bstRoot
        it "returns True for bstRoot contains 10" $ do
            (bstContains bstRoot 10) `shouldBe` True
        it "returns False for bstRoot contains 253" $ do
            (bstContains bstRoot 253) `shouldBe` False

        --bstContains on bstChar
        it "returns True for bstChar contains 'm'" $ do
            (bstContains bstChar 'm') `shouldBe` True
        it "returns False for bstChar contains 'X'" $ do
            (bstContains bstChar 'X') `shouldBe` False

        --bstContains on bstSample
        it "returns True for bstSample contains 1" $ do
            (bstContains bstSample 1) `shouldBe` True
        it "returns True for bstSample contains 7" $ do
            (bstContains bstSample 7) `shouldBe` True
        it "returns False for bstSample contains 20" $ do
            (bstContains bstSample 20) `shouldBe` False

        --bstLCA on bstEmpty
        it "using bstEmpty, returns Nothing as LCA of 2 and 17 (arbitrary values)" $ do
            (bstLCA bstEmpty 2 17) `shouldBe` Nothing

        --bstLCA on bstRoot
        it "using bstRoot, returns Nothing as LCA of 4 and 5 (arbitrary values)" $ do
            (bstLCA bstRoot 4 5) `shouldBe` Nothing
        it "using bstEmpty, returns root (10) as LCA of 10 and 10" $ do
            (bstLCA bstRoot 10 10) `shouldBe` 10

        --bstLCA on bstChar
        it "using bstChar, returns Nothing as LCA of 'X' and 'Y' (arbitrary values)" $ do
            (bstLCA bstChar 'X' 'Y') `shouldBe` Nothing
        it "using bstChar, returns 'c' as LCA of 'a' and 'c' (arbitrary values)" $ do
            (bstLCA bstChar 'a' 'c') `shouldBe` 'c'
        it "using bstChar, returns root ('m') as LCA of 'a' and 't' (arbitrary values)" $ do
            (bstLCA bstChar 'a' 't') `shouldBe` 'm'

        --bstLCA on bstSample
        it "using bstSample, returns Nothing as LCA for 101 and 420 (arbitrary values)" $ do
            (bstLCA bstSample 101 420) `shouldBe` Nothing
        it "using bstSample, returns 9 as LCA for 8 and 10" $ do
            (bstLCA bstSample 8 10) `shouldBe` 9
        it "using bstSample, returns Nothing as LCA for 8 and 1000" $ do
            (bstLCA bstSample 8 1000) `shouldBe` Nothing
