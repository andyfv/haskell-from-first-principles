module Exercises where

-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn n) (TisAn n') = n == n'


-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'


-- 3 
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False


-- 4 
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'


-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'


-- 6
data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

-- 7 
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a')     = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False