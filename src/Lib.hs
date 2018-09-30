module Lib
    ( bstContains, bstLCA, BST, bstEmpty, bstRoot, bstChar, bstSample
    ) where

-- Type: BST
--
-- A type representing a binary search tree.  Any type of element can be stored
-- in the BST as long as it can be totally ordered.
data BST a = Nil | Node a (BST a) (BST a) deriving Show

bstEmpty :: BST Int
bstEmpty = Nil

bstRoot :: BST Int
bstRoot = Node 10 Nil Nil

bstChar :: BST Char
bstChar = Node 'm'
  (Node 'c'
    (Node 'a' Nil Nil)
    Nil)
  (Node 'q' Nil
    (Node 't' Nil Nil))

bstSample :: BST Int
bstSample = Node 7
  (Node 2
    (Node 1 Nil Nil)
    (Node 5
      (Node 4
        (Node 3 Nil Nil)
        Nil)
      (Node 6 Nil Nil)))
  (Node 12
    (Node 9
      (Node 8 Nil Nil)
      (Node 11
        (Node 10 Nil Nil)
        Nil))
    (Node 13 Nil
      (Node 15
        (Node 14 Nil Nil)
        (Node 16 Nil Nil))))


bstContains :: Ord a => BST a -> a -> Bool
bstContains Nil _ = False
bstContains (Node r left right) value =
  if value == r then True
  else if value < r then bstContains left value
  else bstContains right value


bstLCA :: Ord a => BST a -> a -> a -> Maybe (BST a)
-- Base case: The empty tree has no nodes, so the LCA of any two values in an
-- empty tree does not exist.
bstLCA Nil _ _ = Nothing
bstLCA root@(Node r left right) v1 v2 =
  -- If both values are on the same side, descend into that side.
  if v1 < r && v2 < r then bstLCA left v1 v2
  else if v1 > r && v2 > r then bstLCA right v1 v2
  -- If the root value matches at least one node, search the tree for the other
  -- and return this node if we find it.
  else if v1 == r then
       if bstContains root v2 then Just root else Nothing
  else if v2 == r then
       if bstContains root v1 then Just root else Nothing
  -- Otherwise, the values must be on opposite sides of the tree, so search
  -- for both and return the current node if we find them.
  else if (bstContains root v1) && (bstContains root v2) then Just root
  else Nothing
