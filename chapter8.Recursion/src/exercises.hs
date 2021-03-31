module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"




sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = 
    go n 0
    where go num acc
            | num == 0 = acc
            | otherwise = go (num - 1) (acc + num)



multiplication' :: (Integral a) => a -> a -> a
multiplication' x y = 
    go x y
    where go a b
            | a == 0 || b == 0 = 0
            | a == 1 = b
            | b == 1 = a
            | a < 0 = negate $ go (-a) b
            | b < 0 = negate $ go a (-b)
            | otherwise = a + (go (b - 1)) a


mc91 :: (Ord a, Num a) => a -> a
mc91 n 
    | n > 100 = n - 10
    | otherwise = mc91 $ mc91 (n + 11)