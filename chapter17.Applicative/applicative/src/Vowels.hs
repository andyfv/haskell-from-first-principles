module Vowels where

import Control.Applicative
import Test.QuickCheck

stops :: String
stops = "pbtdkg"

vowels :: String 
vowels = "aeiou"

combs :: [a] -> [b] -> [c] -> [(a, b, c)]
combs = liftA3 (,,)