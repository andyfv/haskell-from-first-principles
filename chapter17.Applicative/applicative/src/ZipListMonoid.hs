module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers (EqProp, (=-=))
-- import Test.QuickCheck.Classes


-- instance Monoid a => Monoid (ZipList a) where
--     mempty  = ZipList []
--     mappend = liftA2 mappend


-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--     arbitrary = Sum <$> arbitrary

-- instance Eq a => EqProp (ZipList a) where
--     (=-=) = eq