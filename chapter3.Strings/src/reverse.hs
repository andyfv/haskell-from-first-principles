module Reverse where

rvrs :: String -> String 
rvrs str = let word1 = take 5 str
               word2 = take 2 (drop 6 str)
               word3 = take 7 (drop 9 str)
               in word3 ++ " " ++ word2 ++ " " ++ word1

main :: IO ()
main = print $ rvrs "Curry is awesome!"