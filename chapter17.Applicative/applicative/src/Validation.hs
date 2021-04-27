module Validation where

-- import Data.Validation

data Errors
    = DividedByZero
    | StackOverflow
    | MooglesChewedWires
    deriving (Eq, Show)


success :: Validation [Errors] Integer
success = Success (+1) <*> Success 1
-- success == Success 2


failure :: Validation [Errors] Integer
failure = Success (+1) <*> Failure [StackOverflow]
-- failure == Failure [StackOverflow]


failure' :: Validation [Errors] b
failure' = Failure [StackOverflow] <*> Success (+1)
-- failure' == Failure [StackOverflow]


failures :: Validation [Errors] b
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
-- failures == Failure [MooglesChewedWires, StackOverflow]


-- Exercises

data Validation e a
    = Failure e 
    | Success a 
    deriving (Eq, Show)


instance Functor (Validation e) where
    fmap f (Failure e) = Failure e
    fmap f (Success a) = Success (f a)


instance Monoid e => Applicative (Validation e) where
    pure x = Success x

    (<*>) (Success f) (Success a)  = Success (f a)
    (<*>) (Failure e) (Failure e') = Failure (e <> e')
    (<*>) (Failure e) _ = Failure e
    (<*>) _ (Failure e) = Failure e