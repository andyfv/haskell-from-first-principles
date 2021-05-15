module ChapterExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Prelude hiding (Left, Right)


-- Nope a
data Nope a = NopeDotJpg 
              deriving (Eq, Show)


instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg

    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure

    (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg


instance (Eq a) => EqProp (Nope a) where
    (=-=) = eq

-- PhhhbbtttEither b a
data PhhhbbtttEither b a = Left a | Right b 
                            deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap f (Left a) = Left (f a)
    fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
    pure a = Left a

    (<*>) (Left f) (Left a) = Left (f a)
    (<*>) (Right b) _       = Right b
    (<*>) _ (Right b)       = Right b

instance Monad (PhhhbbtttEither b) where
    return = pure

    (>>=) (Left a)  f = f a
    (>>=) (Right b) _ = Right b


instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = frequency [(3, Left <$> arbitrary), (1, Right <$> arbitrary)]

    
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq


-- Identity a
newtype Identity a = Identity a
                     deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a

    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure

    (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq


-- List a
data List a = Nil | Cons a (List a)
                deriving (Eq, Show)


instance Functor List where
    fmap _ (Nil)        = Nil
    fmap f (Cons a lst) = Cons (f a) (fmap f lst)

instance Semigroup (List a) where
    (<>) Nil as = as
    (<>) as Nil = as
    (<>) (Cons a as) bs = Cons a (as <> bs)


instance Applicative List where
    pure a = Cons a Nil

    (<*>) (Cons f fs) as = (f <$> as) <> (fs <*> as)
    (<*>) Nil _                   = Nil


instance Monad List where
    return = pure

    (>>=) Nil _ = Nil
    (>>=) (Cons a as) f = append (f a) (as >>= f)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)


instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (1, (Cons <$> arbitrary <*> arbitrary))]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq


-----------------------------------------


-- 1.
j :: Monad m => m (m a) -> m a
j as = join as


-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f as = as >>= return . f


-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f as bs = 
    l1 f as <*> bs

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a as fs = fs <*> as


-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)


-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType as = meh as id