module Tree where


import Control.Applicative (liftA3)


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty        = Empty
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
    foldMap _ Empty        = mempty
    foldMap f (Leaf a)     = f a 
    foldMap f (Node left a right) = (foldMap f left) <> f a <> foldMap f right

instance Traversable Tree where
    traverse _ Empty        = pure Empty
    traverse f (Leaf a)     = Leaf <$> f a
    -- traverse f (Node left a right) = Node 
    --                                       <$> traverse f left 
    --                                       <*> f a 
    --                                       <*> traverse f right

    traverse f (Node left a right) = liftA3 Node (traverse f left) 
                                                 (f a) 
                                                 (traverse f right)

