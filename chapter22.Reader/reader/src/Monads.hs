module Monads where

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r 

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)


-- Combine 'foo' and 'bar' in order to increment the values inside the
-- structure and also tell us the length of the value.
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+ 1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r 


frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r


fooBind :: (r -> a) -> (a -> r -> b) -> r -> b
fooBind m k = \r -> k (m r) r


-- Monad instance 
