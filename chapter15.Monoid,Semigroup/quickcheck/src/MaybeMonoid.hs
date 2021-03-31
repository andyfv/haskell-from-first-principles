module MaybeMonoid where

import OptionalMonoid
import Data.Monoid
import Test.QuickCheck


newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where 
    (<>) (First' a) (First' Nada) = First' a
    (<>) (First' Nada) (First' a) = First' a
    (<>) (First' a) _             = First' a

instance Monoid a => Monoid (First' a) where
    mempty = First' Nada

    mappend (First' a) (First' Nada) = First' a
    mappend (First' Nada) (First' a) = First' a
    mappend (First' a) _             = First' a


firstGen :: Arbitrary a => Gen (First' a) 
firstGen = do
    a <- arbitrary
    frequency [ (1, return (First' Nada))
              , (1, return (First' (Only a)))
              ]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = firstGen

firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend


type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool
