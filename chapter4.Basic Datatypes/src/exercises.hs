module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst =
    (reverse lst) == lst

myAbs :: Integer -> Integer
myAbs n = if n < 0
          then (negate n)
          else n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2)) 


x = (+)

addOneToLength :: [a] -> Int
addOneToLength xs = w `x` 1
                    where w = length xs


id' :: a -> a
id' x = x

fst' :: (a, b) -> a
fst' (a, b) = a