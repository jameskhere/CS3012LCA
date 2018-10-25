module Lib
    ( Path, empty, cons, dagContains, dagLCA
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

dagContains :: Path -> Id -> Bool
dagContains (xs :# n) a = a `elem` xs

-- gets LCA of two paths by cutting off at same height (if uneven) and going
-- through the list (up in tree) to find the first common ancestor
dagLCA :: Path -> Path -> Id
dagLCA ([] :# 0) _ = 0
dagLCA _ ([] :# 0) = 0
dagLCA (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys)
    | x == y   = x
    | otherwise = go (n - 1) xs ys
