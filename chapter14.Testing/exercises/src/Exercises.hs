module Exercises where

import Data.List
import Data.Char

-- 1
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half



-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
    snd $ foldr go (Nothing, True) xs
        where go _ status@(_, False) = status
              go y (Nothing, t)      = (Just y, t)
              go y (Just x, t)       = (Just y, x >= y)


-- 3.1. Addition : associative
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = 
    x + (y + z) == (x + y) + z


-- 3.2. Addition : commutative 
plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = 
    x + y == y + x


-- 4.1. Multiplication : distributive
multDistributive :: (Eq a, Num a) => a -> a -> a -> Bool
multDistributive x y z =
    x * (y + z) == (x * y) + (x * z)

-- 4.2. Multiplication : commutative
multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
    x * y == y * x

-- 4.3. Multiplication : associative
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
    (x * y) * z == x * (y * z)


-- 4.4. Multiplication : identity
multIdentity :: (Eq a, Num a) => a -> Bool
multIdentity x =
    x * 1 == 1 * x


-- 5.1. quot And rem
quotRem :: Integral a => a -> a -> Bool
quotRem x y =
    (quot x y) * y + (rem x y) == x


-- 5.2. div And mod
divMod :: Integral a => a -> a -> Bool
divMod x y = 
    (div x y) * y + (mod x y) == x


-- 6.1. Power : associative 
powerAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
powerAssociative x y z =
    (x ^ y) ^ z == x ^ (y ^ z)


-- 6.2. Power : commutative
powerCommutative :: Integral b => b -> b -> Bool
powerCommutative x y =
    x ^ y == y ^ x


-- 7. Reverse List Twice
reverseListTwice :: Eq a => [a] -> Bool
reverseListTwice xs = 
    (reverse . reverse) xs == id xs


-- 8.1. Function Application ($)
dollar :: (Eq b) => (a -> b) -> a -> Bool
dollar f x =
    f x == (f $ x)

-- 8.2. Function Composition (.)
dot :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
dot f g x = 
    (f . g) x == f (g x)


-- 9.1. foldr (:) == (++)
foldrConsWithPlusPlus :: Eq a => [a] -> [a] -> Bool
foldrConsWithPlusPlus xs ys =
    foldr (:) xs ys == (xs ++ ys)

-- 9.2. foldr (++) [] == concat
foldrPlusPlusWithConcat :: [[Int]] -> Bool
foldrPlusPlusWithConcat xs =
    foldr (++) [] xs == concat xs


-- 10. 
f :: Int -> [a] -> Bool
f n xs = 
    length (take n xs) == n


-- 11. read show
readShow :: (Eq a, Read a, Show a) => a -> Bool
readShow x = 
    (read (show x)) == x



-- Failure
square :: Num a => a -> a
square x = x * x

squareIdentity :: (Floating a, Eq a) => a -> Bool
squareIdentity x = 
    (square . sqrt) x == x


-- Idempotence
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice 

idempotence :: String -> Bool
idempotence x = 
    (capitalizeWord x == twice capitalizeWord x) 
    && 
    (capitalizeWord x == fourTimes capitalizeWord x)


idempotence' :: Ord a => [a] -> Bool
idempotence' x =
    (sort x == twice sort x)
    && 
    (sort x == fourTimes sort x)


capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs 
