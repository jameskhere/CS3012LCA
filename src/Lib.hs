module Lib
    ( bstContains, bstLCA, BST, bstEmpty, bstRoot, bstChar, bstSample
    ) where


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
