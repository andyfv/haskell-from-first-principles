module List where

myWords :: [Char] -> [[Char]]
myWords [] = []
myWords (' ': xs) = myWords xs
myWords str = takeWhile (/= ' ') str : myWords (dropWhile (/= ' ') str)



-- >>> let acro xs = [x | x <- xs, elem x ['A'..'Z']]
-- >>> acro "Self Contained Underwater Breathing Apparatus" 
-- "SCUBA"
--
