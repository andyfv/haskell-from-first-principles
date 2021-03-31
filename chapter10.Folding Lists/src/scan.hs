module Scan where

fibs    = 1 : scanl (+) 1 fibs

-- Take first N Fibb numbers
-- fibsN x = take x fibs


-- Take only the Fibb number that are less than 100
fibs
    | 
