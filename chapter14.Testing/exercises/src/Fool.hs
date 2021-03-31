module Fool where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
    oneof [return Fulse, return Frue]

instance Arbitrary Fool where
    arbitrary = foolGen