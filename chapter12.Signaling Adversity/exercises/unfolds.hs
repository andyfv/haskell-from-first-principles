module Unfolds where

-- 1. iterate
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f nextA
    where nextA = f a


-- 2. unfoldr
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f seed = case f seed of
    Just (a,b) -> a : myUnfoldr f b
    Nothing    -> []


-- 3. better iterate
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b,f b)) x