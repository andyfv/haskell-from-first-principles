module Section9_10 where

multiplesOf3 = filter (\x -> (rem x 3) == 0) [1..30]

multiplesOf3V2 = length . filter ((== 0) . (`rem` 3)) $ [1..30]

filterArticles = filter (`notElem` ["a", "am", "the"]) . words