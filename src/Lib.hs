module Lib
    ( Path, empty, cons, dagLCA
    ) where

-- Path is a list with associated length
type Id = Int
data Path = [Id] :# !Int

-- path has no entry in the graph and a length of 0
empty :: Path
empty = [] :# 0

-- adds new "node" to front of list - last entry in list is the "root"
cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

-- gets LCA of two paths by cutting off at same height (if uneven) and going
-- through the list (up in tree) to find the first common ancestor
dagLCA :: Path -> Path -> Path
dagLCA (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys)
    | x == y   = xxs :# n
    | otherwise = go (n - 1) xs ys
