{-# LANGUAGE FlexibleInstances #-}

module NewType where

-- data Goats = Goats Int deriving (Eq, Show)
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)


tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 42

instance TooMany (Int, String) where
    tooMany (n, str) = n > 42

instance TooMany (Int, Int) where
    tooMany (x, y) = (x + y) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany x || tooMany y