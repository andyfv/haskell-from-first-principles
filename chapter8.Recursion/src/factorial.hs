module Factorial where

factorial :: Integer -> Integer 
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0

three' = (inc . inc . inc) 0