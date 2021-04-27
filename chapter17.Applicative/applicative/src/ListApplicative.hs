{-# LANGUAGE FlexibleContexts #-}
module ListApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a)
                deriving (Eq, Show)

instance Functor List where
    fmap f Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Semigroup (List a) where
    (<>) Nil ys         = ys
    (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Applicative List where
    pure a      = Cons a Nil
    
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) as = (fmap f as) <> (fs <*> as)


fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

take' :: Int -> List a -> List a 
take' n xs = go n Nil xs 
       where go 0 acc _   = acc
             go n acc Nil = acc
             go n acc (Cons x xs) = go (n - 1) (Cons x acc) xs


-- ZipList Applicative

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
                where xs' = let (ZipList' l) = xs
                            in take' 3000 l
                      ys' = let (ZipList' l) = ys
                            in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' (fmap f xs)


zipListWith :: (a -> b -> c) -> List a -> List b -> List c
zipListWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipListWith f xs ys)
zipListWith _ _ _ = Nil

instance Applicative ZipList' where
    pure a = ZipList' (Cons a Nil)

    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' xs) (ZipList' ys) = ZipList' (zipListWith ($) xs ys)