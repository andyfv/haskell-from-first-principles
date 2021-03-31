module Section10_6 where

f :: String -> String -> String
f a b = take 3 a ++ b

result :: String 
result = foldr f "" ["Pizza", "Apple", "Banana"] 

-- 