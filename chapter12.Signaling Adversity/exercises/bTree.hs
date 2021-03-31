module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)


-- 1. unfold for BinaryTree
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f seed = case f seed of
    Just (l,m,r) -> Node (unfold f l) m (unfold f r)
    Nothing      -> Leaf


-- 2. Build a Tree
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (checkDelta n) 0
    where checkDelta height nRoot
            | height > nRoot = Just (nRoot + 1, nRoot, nRoot + 1)
            | otherwise      = Nothing
