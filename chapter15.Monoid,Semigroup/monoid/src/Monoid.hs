module Monoid where

import Data.Monoid (Sum)

15.5 List

mappend [1,2,3] [4,5,6]
[1,2,3,4,5,6]

mconcat [[1..3], [4..6]]
[1,2,3,4,5,6]

mappend "Trout" " goes well with gralic"
"Trout goes well with gralic"

(++) [1,2,3] [4,5,6]
[1,2,3,4,5,6]

(++) "Trout" " goes well with garlic"
"Trout goes well with garlic"

foldr (++) [] [[1..3],[4..6]]
[1,2,3,4,5,6]

foldr mappend mempty [[1..3], [4..6]]
[1,2,3,4,5,6]

instance Monoid [a] where
    mempty  = []
    mappend = (++)




15.6. Monoid for Integers

mappend (Sum 1) (Sum 5)
Sum {getSum = 6}

mappend (Product 5) (Product 5)
Product {getProduct = 25}

mappend (Sum 4.5) (Sum 3.4)
Sum {getSum = 7.9}



Sum and Product

:t (<>)
(<>) :: Semigroup a => a -> a -> a

(Sum 9) <> (Sum 8) 
Sum {getSum = 17}



Sum and Product Accessors

getSum $ mappend (Sum 1) (Sum 1)
2

getProduct $ mappend (Product 1) (Product 5)
5

getSum $ mconcat [(Sum 5), (Sum 6), (Sum 7)]
18