module FuncInstances where

import Test.QuickCheck


-- 1. Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
    fmap f (Identity a ) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do Identity <$> arbitrary


-- 2. Pair a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do 
        a  <- arbitrary
        a' <- arbitrary
        return (Pair a a')


-- 3. Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)


-- 4. Three a b c 
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)


-- 5. Three' a b = Three' a b b

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a  <- arbitrary
        b  <- arbitrary
        b' <- arbitrary
        return (Three' a b b')    


-- 6. Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
            arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                d <- arbitrary
                return (Four a b c d)


-- 7. Four' a b = Four' a a a b
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        a3 <- arbitrary
        b  <- arbitrary
        return (Four' a1 a2 a3 b)


-- 8. Trivial - None
data Trivial = Trivial





