module Predicate where

pred1 = [x^y | x <- [1..10], y <- [2, 3], x^y < 200]

tup = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]

mySqr = [x^2 | x <- [1..10]]
mySqr2 = [(x,y) | x <- mySqr, y <- [1..3], x <= 4]

