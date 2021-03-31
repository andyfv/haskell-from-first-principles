module Section9_12 where

import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirstChar :: String -> String
capitalizeFirstChar [] = []
capitalizeFirstChar (x:xs) = toUpper x : xs 


capitalize :: String -> String
capitalize [] = []
capitalize (x: xs) = toUpper x : capitalize xs


getFirstChar :: String -> Char
getFirstChar = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) 
    | x == True = True
    | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) 
    | f x == True = True
    | otherwise   = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y    = True
    | otherwise = myElem x ys

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x] 

squish :: [[a]] -> [a]
squish []      = []
squish (x: xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []  = []
squishMap f lst = concat $ map f lst

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f (x:xs) =
    go f x xs 
    where go _ max [] = max
          go fun max (x:xs)
            | fun max x == LT = go fun x xs
            | otherwise       = go fun max xs 

myMax :: Ord a => [a] -> a
myMax = myMaximumBy compare

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x:xs) =
    go f x xs 
    where go _ max [] = max
          go fun max (x:xs)
            | fun max x == GT = go fun x xs
            | otherwise       = go fun max xs 

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' _ [x] = x
myMinimumBy' f (x:y:xs) | f x y == LT = myMinimumBy' f y xs

myMin :: Ord a => [a] -> a
myMin = myMinimumBy compare