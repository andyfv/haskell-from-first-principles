module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eigth"
    | n == 9 = "nine"
    | otherwise = "non-existant"


digits :: Int -> [Int]
digits n =
    go n []
    where go num res
            | num < 10 = [num] ++ res
            | otherwise = go (div num 10) ([mod num 10] ++ res)



wordNumber :: Int -> String
wordNumber n =
    concat . intersperse "-" $ map digitToWord (digits n)