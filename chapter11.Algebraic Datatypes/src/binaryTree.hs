module BinaryTree where

data BinaryTree a 
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
    Node (mapTree f left) (f a) (mapTree f  right)


testTree :: BinaryTree Integer
testTree =
    Node
        (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

-- Preorder
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = 
    [a] ++ (preorder left) ++ (preorder right)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Preorder : Bad news"


-- Inorder
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
    (preorder left) ++ [a] ++ (preorder right)

testInorder :: IO ()
testInorder =
    if inorder testTree == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "Inorder : Bad news"

-- PostOrder
postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) =
    (postOrder left) ++ (postOrder right) ++ [a]


testPostorder :: IO ()
testPostorder =
    if postOrder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Postorder : Bad news"


-- TreeFoldr
foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f b Leaf = b
foldrTree f b (Node left a right) = 
    f a (foldrTree f (foldrTree f b left) right)

testTree' :: BinaryTree Integer
testTree' =
    Node 
        (Node Leaf 3 Leaf)
        1 
        (Node Leaf 4 Leaf)


mapExpected =
    Node 
        (Node Leaf 4 Leaf)
        2
        (Node Leaf 5 Leaf)

mapOkay = 
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"