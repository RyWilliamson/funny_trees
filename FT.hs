module FT where

data FunnyTree a = FunnyNode a [FunnyTree a] deriving (Show,Eq)

treeNodeCount :: FunnyTree a -> Int
treeNodeCount (FunnyNode _ []) = 1
treeNodeCount (FunnyNode _ subTree) = 1 + sum (map treeNodeCount subTree)

treeDepth :: FunnyTree a -> Int
treeDepth (FunnyNode _ []) = 1
treeDepth (FunnyNode _ subTree) = 1 + maximum (map treeDepth subTree)

treeMap :: (a->b) -> FunnyTree a -> FunnyTree b
treeMap f (FunnyNode x _) = FunnyNode (f x) []

-- Depth first fold
treeFold :: (a->b->b) -> b -> FunnyTree a -> b
treeFold fun acc tree = acc