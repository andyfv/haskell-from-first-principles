module StringProcessing where

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