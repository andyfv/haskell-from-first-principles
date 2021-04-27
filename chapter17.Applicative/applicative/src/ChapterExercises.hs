module ChapterExercises where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1. []

-- pure :: a -> [] a
-- (<*>) :: [(a -> b)] -> [a] -> [b]



-- 2. IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a - > IO b



-- 3. (,) a
-- pure :: a -> (t, a)
-- (<*>) :: (t, (a -> b)) -> (t, a) - > (t, b)



-- 4. (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)  



-- 1. Pair a 
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a0 a1) = Pair (f a0) (f a1)

instance Applicative Pair where
    pure a = Pair a a

    (<*>) (Pair f0 f1) (Pair a0 a1) = Pair (f0 a0) (f1 a1)

instance Monoid a => Semigroup (Pair a) where
    (<>) (Pair a0 a1) (Pair a2 a3) = Pair (a0 <> a2) (a1 <> a3)


instance Monoid a => Monoid (Pair a) where
    mempty = Pair mempty mempty

    mappend (Pair a0 a1) (Pair a2 a3) = Pair (a0 <> a2) (a1 <> a3)


instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do 
        a0 <- arbitrary
        a1 <- arbitrary
        return (Pair a0 a1)


-- instance Eq a => EqProp (Pair a) where
--     (=-=) = eq



-- 2. Two a b
data Two a b = Two a b deriving Show

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b

    (<*>) (Two f g) (Two a b) = Two (f <> a) (g b)




-- 3. Three a b c
data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c

    (<*>) (Three f g h) (Three a b c) = Three (f <> a) (g <> b) (h c)


-- 4. Three' a b = Three' a b b 
data Three' a b = Three' a b b deriving Show

instance Functor (Three' a) where
    fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

instance (Monoid a) => Applicative (Three' a) where
    pure b = Three' mempty b b

    (<*>) (Three' f g0 g1) (Three' a b0 b1) = 
           Three' (f <> a) (g0 b0) (g1 b1)



-- 5. Four a b c d = Four a b c d 
data Four a b c d = Four a b c d deriving Show

instance Functor (Four a b c ) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure c = Four mempty mempty mempty c

    (<*>) (Four f g h j) (Four a b c d) = 
        Four (f <> a) (g <> b) (h <> c) (j d)


-- 6. Four' a b = Four a a a b
data Four' a b = Four' a a a b deriving Show

instance Functor (Four' a) where
    fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b

    (<*>) (Four' f0 f1 f2 g) (Four' a0 a1 a2 b) = 
        Four' (f0 <> a0) (f1 <> a1) (f2 <> a2) (g b)
