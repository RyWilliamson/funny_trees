module FT where

data FunnyTree a = FunnyNode a [FunnyTree a] deriving (Show,Eq)

-- Takes sum of the nodes in each subTree recursively.
treeNodeCount :: FunnyTree a -> Int
treeNodeCount (FunnyNode _ []) = 1
treeNodeCount (FunnyNode _ subTree) = 1 + sum (map treeNodeCount subTree)

-- Takes the max of the depths for each subTree it has recursively.
treeDepth :: FunnyTree a -> Int
treeDepth (FunnyNode _ []) = 1
treeDepth (FunnyNode _ subTree) = 1 + maximum (map treeDepth subTree)

-- Applies f to the node value then maps recursively to subTrees.
treeMap :: (a->b) -> FunnyTree a -> FunnyTree b
treeMap f (FunnyNode x []) = FunnyNode (f x) []
treeMap f (FunnyNode x subTree) = FunnyNode (f x) (map (treeMap f) subTree)

-- Uses prelude foldr over flattened tree.
treeFold :: (a->b->b) -> b -> FunnyTree a -> b
treeFold f acc tree = foldr f acc (treeFlatten tree)

-- Converts the tree to a list depth-first.
treeFlatten :: FunnyTree a -> [a]
treeFlatten (FunnyNode x []) = [x]
treeFlatten (FunnyNode x subTree) = x : concat (map treeFlatten subTree)