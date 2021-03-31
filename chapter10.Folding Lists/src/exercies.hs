module Exercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combine :: [(Char, Char, Char)]
combine = [(x,y,z)| x <- stops, y <- vowels, z <- stops, x == 'p', z == 'p']


-- myOR
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) 
    | x == True = True
    | otherwise = myOr xs

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs

myOr3 :: [Bool] -> Bool
myOr3 = foldr (\x y -> if x == True then True else y) False

myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False


-- myANY
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x == True = True
    | otherwise   = myAny f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = f x || myAny2 f xs

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr ((||) . f) False 

-- myElem
myElem :: Eq a => a -> [a] -> Bool
myElem el = myAny3 (== el)

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 el = foldr ((||) . (== el)) False


-- myReverse
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:). f) []

-- myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a:b else b) []

-- squish
squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

-- squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) [] 

-- squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximuxBy
myMaximuxBy :: (a -> a -> Ordering) -> [a] -> a
myMaximuxBy f (x:xs) = foldl (\a b -> if f a b == GT then a else b) x xs

 
-- myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\a b -> if f a b == GT then b else a) x xs