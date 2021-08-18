module Examples where

sequanceA' :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequanceA' = traverse id


-- >>> sum [1,2,3]
-- 6
-- 
-- >>> fmap sum [Just 1, Just 2, Just 3]
-- [1,2,3]
-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
--
-- >>> :t Just 
-- Just :: a -> Maybe a
-- >>> :t sum
-- sum :: (Foldable t, Num a) => t a -> a
-- >>> sum (Just 1)
-- 1
--
-- sum :: (Foldable t, Num a) => t a -> a
-- sum ta = foldl' (+) 0 ta


-- >>> (fmap . fmap) sum Just [1,2,3]
-- Just 6
-- 
-- >>> :t (fmap . fmap)
-- (fmap . fmap) :: (a -> b) -> f1 (f2 a) -> f1 (f2 b)
--
-- >>> :t fmap 
-- fmap :: (a -> b) -> f a -> f b
-- fmap . fmap :: (a -> b) -> f1 ()


-- >>> fmap product [Just 1, Just 2, Nothing]
-- [1,2,1]
-- >>> product Nothing
-- 1
--

-- >>> :t product
-- product :: (Foldable t, Num a) => t a -> a
--



------------------------------------------------------------------------------
-- traverse :: (Applicative f , Traversable t) => (a -> f b) -> t a -> f (t b)
-- traverse = sequeanceA . fmap f

-- fmap     :: (a -> b)   -> f a -> f b
-- (=<<)    :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)