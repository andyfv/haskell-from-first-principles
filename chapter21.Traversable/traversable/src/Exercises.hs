module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative (liftA3)

----------------- Identity -----------------
newtype Identity a = Identity a 
                        deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity a) = f a
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq


----------------- Constant -----------------
newtype Constant a b = Constant { getConstant :: a }
                        deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = Constant <$> pure a

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
    (=-=) = eq


----------------- Maybe -----------------
data Optional a = Nada | Yada a
                  deriving (Eq, Show, Ord)

instance Functor Optional where
    fmap f (Yada a) = Yada (f a)
    fmap _ Nada     = Nada

instance Foldable (Optional) where
    foldMap f (Yada a) = f a
    foldMap _ Nada     = mempty

instance Traversable (Optional) where
    traverse f (Yada a) = Yada <$> f a
    traverse _ Nada     = pure Nada 

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do 
        a <- arbitrary 
        elements [Nada, Yada a]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq


----------------- List -----------------
data List a = Nil | Cons a (List a)
              deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
    foldr _ acc Nil = acc
    foldr f acc (Cons a as) = f a (foldr f acc as)


instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a  <- arbitrary
        as <- arbitrary
        elements [Nil, Cons a as]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq


----------------- Three -----------------
data Three a b c = Three a b c
                   deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f acc (Three _ _ c) = f c acc

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq


----------------- Pair -----------------
data Pair a b = Pair a b
                deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldr f acc (Pair _ b) = f b acc

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq


----------------- Big -----------------
data Big a b = Big a b b
               deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b0 b1) = Big a (f b0) (f b1)

instance Foldable (Big a) where
    foldMap f (Big _ b0 b1) = f b0 <> f b1

instance Traversable (Big a) where
    traverse f (Big a b0 b1) = Big a <$> f b0 <*> f b1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        a  <- arbitrary
        b0 <- arbitrary
        b1 <- arbitrary
        return (Big a b0 b1)

instance (Eq a, Eq b) =>  EqProp (Big a b) where
    (=-=) = eq


----------------- Bigger -----------------
data Bigger a b = Bigger a b b b
                  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b0 b1 b2) = Bigger a (f b0) (f b1) (f b2)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ b0 b1 b2) = (f b0) <> (f b1) <> (f b2)

instance Traversable (Bigger a) where
    -- traverse f (Bigger a b0 b1 b2) = Bigger a <$> f b0 <*> f b1 <*> f b2
    traverse f (Bigger a b0 b1 b2) = liftA3 (Bigger a) (f b0) (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = do
        a  <- arbitrary
        b0 <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        return (Bigger a b0 b1 b2)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq
