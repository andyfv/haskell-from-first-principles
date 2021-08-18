module Exercises where


-- 1. sum
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs


-- 2. product
product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 1 xs


-- 3. elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = foldr (\a acc -> acc || x == a) False xs


-- 4. minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a 
minimum' xs = 
    foldr fun Nothing xs 
    where
        fun x Nothing  = Just x
        fun x (Just y) = Just (min x y)


-- 5. maximum
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = 
    foldr fun Nothing xs
    where 
        fun x Nothing  = Just x
        fun x (Just y) = Just (max x y)


-- 6. null . Return the initial value if the structure is empty
null' :: (Foldable t) => t a -> Bool
null' xs = foldr (\_ _ -> False) True xs


-- 7. length
length' :: (Foldable t) => t a -> Int
length' xs = foldr (\_ acc -> acc + 1) 0 xs


-- 8. toList
toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (\a acc -> a : acc) ([]) xs 


toList'' :: (Foldable t) => t a -> [a]
toList'' xs = foldMap (\x -> [x]) xs


-- 9. fold
fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap (id) xs


-- 10. foldMap
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x acc -> (f x) <> acc) mempty xs


------------------------------------------------------
-- 1. Constant a b = Constant b
data Constant a b = Constant b deriving (Eq, Show)

instance Semigroup b => Semigroup (Constant a b) where
    (<>) (Constant x) (Constant y) = (Constant (x <> y))

instance (Monoid b, Semigroup b) => Monoid (Constant a b) where
    mempty = Constant mempty

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b


-- 2. Two a b = Two a b
data Two a b = Two a b 
                deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a0 b0) (Two a1 b1) = Two (a0 <> a1) (b0 <> b1)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z


-- 3. Three a b c = Three a b c
data Three a b c = Three a b c
                    deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a0 b0 c0) (Three a1 b1 c1) = Three (a0 <> a1)
                                                   (b0 <> b1)
                                                   (c0 <> c1)


instance (Monoid a , Monoid b , Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty


instance Foldable (Three a b) where
    -- foldMap f (Three _ _ c) = f c
    foldr f z (Three _ _ c) = f c z


-- 4. data Three' a b = Three a b b 
data Three' a b = Three' a b b 
                    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
    (<>) (Three' a0 b0 b'0) (Three' a1 b1 b'1) = Three' (a0 <> a1)
                                                        (b0 <> b1)
                                                        (b'0 <> b'1)


instance (Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty


instance Foldable (Three' a) where
    foldMap f (Three' _ b0 b1) = (f b0) <> (f b1)


-- 5. data Four a b = Four a b b b
data Four a b = Four a b b b
                    deriving (Eq, Show)


instance Foldable (Four a) where
    foldMap f (Four _ b0 b1 b2) = (f b0) <> (f b1) <> (f b2) 


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = 
    foldMap fun
    where fun x = 
            if f x
            then pure x
            else mempty
