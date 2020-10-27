module FT where

data FunnyTree a = FunnyNode a [FunnyTree a] deriving (Show,Eq)

treeNodeCount :: FunnyTree a -> Int
treeNodeCount (FunnyNode _ []) = 1
treeNodeCount (FunnyNode _ subTree) = 1 + sum (map treeNodeCount subTree)

treeDepth :: FunnyTree a -> Int
treeDepth (FunnyNode _ []) = 1
treeDepth (FunnyNode _ subTree) = 1 + maximum (map treeDepth subTree)

treeMap :: (a->b) -> FunnyTree a -> FunnyTree b
treeMap f (FunnyNode x []) = FunnyNode (f x) []
treeMap f (FunnyNode x subTree) = FunnyNode (f x) (map (treeMap f) subTree)

-- Depth first fold
treeFold :: (a->b->b) -> b -> FunnyTree a -> b
treeFold f acc tree = foldr f acc (treeFlatten tree)

treeFlatten :: FunnyTree a -> [a]
treeFlatten (FunnyNode a []) = [a]
treeFlatten (FunnyNode a subTree) = a: concat (map treeFlatten subTree)