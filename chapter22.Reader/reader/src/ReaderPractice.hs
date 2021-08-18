module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' key ((k, v):rest)
    | key == k  = Just v
    | otherwise = lookup' key rest


-- zip 'x' and 'y' using 3 as a lookup key
xs :: Maybe Integer
xs = lookup' 3 $ zip x y


-- zip 'y' and 'z' using 6 as a lookup key
ys :: Maybe Integer
ys = lookup' 6 $ zip y z


-- zip 'x' and 'y' using 4 as a lookup key
zs :: Maybe Integer
zs = lookup' 4 $ zip x y


-- zip 'x' and 'z' using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z


-- make a tuple of 'xs' and 'ys'
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys


-- make a tuple of 'ys' and 'zs'
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs 


-- take an input and make a tuple of of the result of two applications of
-- z' from above
x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> z' n  <*> z' n


uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b 


summed :: Num c => (c, c) -> c
summed = uncurry' (+)


bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)


fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just a) = a
fromMaybe' a Nothing  = a


main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequA 7


sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

