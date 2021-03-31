module VigenereCipher 
    (vigenere)
    where

import Data.Char

vigenere :: String -> String -> String
vigenere [] _  = []
vigenere xs [] = xs
vigenere message keyword =
    encrypt (map toUpper message) (map toUpper keyword)

encrypt :: String -> String -> String
encrypt [] _ = []
encrypt m [] = m
encrypt (x:xs) key@(k:_)
    | isLetter x == False = toUpper x : encrypt xs key
    | otherwise = newChar x k : encrypt xs (newKey key)
    where
        newChar a b = toUpper $ chr $ 65 + mod (ord a + ord b) 26
        newKey key' = drop 1 key' ++ take 1 key'
