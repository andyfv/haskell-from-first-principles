module SplitAtChar where

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n" 
thirdSen  = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitAtChar :: Char -> String -> [String]
splitAtChar _ [] = []
splitAtChar ch (x: xs) 
    | ch == x   = splitAtChar ch xs
    | otherwise = takeWhile (/= ch) str : splitAtChar ch (dropWhile (/= ch) str)
    where str = x: xs
