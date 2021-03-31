module AsPattern where

import Data.Char


f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t


doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x:xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xt) (y:yt)
    | x == y    = isSubseqOf xt yt
    | otherwise = isSubseqOf xs yt


-- Capitalize sentences
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map makePair . words

makePair :: String -> (String, String)
makePair str = (str, capitalize str)

capitalize :: String -> String
capitalize (x : xs) = (toUpper x) : xs


-- Capitalize paragraphs
capitalizeParagraph :: String -> String
capitalizeParagraph str = 
    go (capitalize str)
    where
        go [] = []
        go ('.' : ' ' : xs) = ". " ++ (go $ capitalize xs)
        go (x:xs) = x : go xs
