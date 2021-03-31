module Exercises where

import Data.List

{- 
    1. Given :
    id' :: a -> a

    What is the kind of a ?
    Answer :     * -> *
-} 


{- 
    2. What are the kinds of a and f?
    r :: a -> f a
    
    Answer : 
    - for (a)   :  *
    - for (f a) :  * -> *
-} 



-- String Processing

{-
    1. Write a recursive function named replaceThe which takes
    a text/string, breaks it into words and replaces each instance 
    of “the” with “a”.
-}
replaceThe :: String -> String
replaceThe []  = []
replaceThe str = unwords . check $ words str
    where check []     = []
          check (w:ws) = case notThe w of
              Just s  -> s : check ws
              Nothing -> "a" : check ws


notThe :: String -> Maybe String
notThe word = 
    case word /= "the" of
        True   -> Just word
        False  -> Nothing

{-
    Write a recursive function that takes a text/string, 
    breaks it into words, and counts the number of instances 
    of ”the” followed by a vowel-initial word.
-}
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 $ words str
    where go n []  = 0
          go n [x] = n
          go n (word1:word2@(word2Head:_):rest) = case notThe word1 of
                Just w  -> go n (word2:rest)
                Nothing -> if isVowel word2Head 
                           then go (n + 1) rest 
                           else go n (word2:rest)


isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"



{-
    3. Return the number of letters that are vowels in a word.
-}
countVowels :: String -> Integer
countVowels = foldl f 0 
    where f acc c = if isVowel c then acc + 1 else acc


-- Validate the Word

{-
    Use the Maybe type to write a function that counts the 
    number of vowels in a string and the number of consonants. 
    If the number of vowels exceeds the number of consonants, 
    the function returns Nothing.
-}
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = if   countConsonants str > countVowels str
             then Just (Word' str)
             else Nothing 


countConsonants :: String -> Integer
countConsonants = foldl f 0 
    where f acc c = if not $ isVowel c then acc + 1 else acc




-- Natural Numbers

{- 
    Valid Natural numbers: from 0 to infinity

    (Integer (Natural)) Integers is a superset of Natural
        - Any Natural can be represented by an Integer
        - Negative Integers can not be represented by Naturals

    Implement functions to:
        - convert Naturals to Integers (Nat -> Integer)
        - convert Integers to Naturals (Integer -> Nat)
-}
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Zero)   = 0
natToInteger (Succ n) = 1 + natToInteger n


integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0     = Nothing
    | otherwise = Just $ mkNat n
    where mkNat 0 = Zero
          mkNat i = Succ (mkNat $ i - 1)


-- Small library for Maybe 
