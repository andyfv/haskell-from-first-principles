module Cipher 
    (encrypt)
    where

import Data.Char

cipher :: Char -> Int -> Char -> Char
cipher char shift base = 
    chr (start + delta)
    where start = ord base
          delta = mod (ord char + shift - start) 26

encrypt :: Int -> String -> String
encrypt _ [] = []
encrypt shift (x:xs)
    | isAsciiLower x = (cipher x shift 'a') : encrypt shift xs
    | isAsciiUpper x = (cipher x shift 'A') : encrypt shift xs
    | otherwise      = x : encrypt shift xs


decrypt :: Int -> String -> String
decrypt shift = encrypt (-shift)
