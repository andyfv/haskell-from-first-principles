module Phone where

import Data.Char
import Data.List


-- Keyboard
type Letters   = [Char]
data Symbol    = Symbol Char deriving Show
data Key       = Key Symbol Letters
data Phone     = Phone [Key]


-- Keyboard Char representation
type Presses    = Int
data DigitCombo = DigitCombo Symbol Presses deriving Show

-- instance Show DigitCombo where
--     show (DigitCombo (Symbol s) p) = 


phone :: Phone
phone = Phone
    [ Key (Symbol '1') "1"
    , Key (Symbol '2') "abc2"
    , Key (Symbol '3') "def3"
    , Key (Symbol '4') "ghi4"
    , Key (Symbol '5') "jkl5"
    , Key (Symbol '6') "mno6"
    , Key (Symbol '7') "pqrs7"
    , Key (Symbol '8') "tuv8"
    , Key (Symbol '9') "wxyz9"
    , Key (Symbol '0') " 0"
    , Key (Symbol '*') "^*"
    , Key (Symbol '#') ".,#"
    ]


reverseTaps :: Phone -> String -> [DigitCombo]
reverseTaps _ []      = []
reverseTaps phone str = concatMap (transformKey phone) str


{- Transform a single character to a combination of
    (Key, Number of presses needed)
-}
transformKey :: Phone -> Char -> [DigitCombo]
transformKey phone c 
    | isUpper c = (DigitCombo (Symbol '*') 1) : getDigit phone (toLower c) 
    | otherwise = getDigit phone c


{- Helper function to get the combination of
    (Key, Number of presses needed)
-}
getDigit :: Phone -> Char -> [DigitCombo]
getDigit (Phone []) _ = error "Error: no such symbol" 
getDigit (Phone ((Key s letters):rest)) c =
    case elemIndex c letters of
            Just n -> [DigitCombo s (n + 1)]
            _      -> getDigit (Phone rest) c

{- Transform a list of sequence 
-}
toDigits :: [DigitCombo] -> String
toDigits digits = 
    concatMap (\(DigitCombo (Symbol s) p) -> replicate p s) digits


-- No idea what this is supposed to be !?
-- cellPhonesDead :: Phone -> String -> [(Digit, Presses)]

convo :: [String]
convo = 
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

-- 3. How many times do digits need to be pressed for eacg message?
fingerTaps :: [DigitCombo] -> Presses
fingerTaps = foldl (\acc (DigitCombo _ p) -> acc + p) 0 


-- 4. What was the most popular letter for each message? 
mostPopularLetter :: String -> Char
mostPopularLetter = 
    snd                                     -- Return just the char
    . maximum                               -- Get the tuple containing the most common char
    . map (\xs -> (length xs, head xs))     -- Transform the groups to tuples
    . group                                 -- Group the same chars together
    . sort                                  -- Sort the list
    . filter isLetter                       -- Filter out non-letters


-- 5. What is the most popular letter overall?
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

-- What is the most popular word?
coolestWord :: [String] -> String
coolestWord = 
    snd                                     -- Return just the most common word
    . maximum                               -- Get the tuple containing the most common word
    . map (\xs -> (length xs, head xs))     -- Transform the groups to tuples (lengthOfGroup,word)
    . group                                 -- Group the same words together
    . sort                                  -- Sort the words
    . words                                 -- Break string into words
    . map toLower                           -- Lower the cases
    . filter (not . isPunctuation)          -- Filter out punctuation
    . intercalate " "                       -- Concat all strings and add empty space between them 


-- Get transfromed list of messages in the form of [DigitCombo] 
taps = map (reverseTaps phone) convo

-- Get number of presses for each message in convo
presses = map fingerTaps taps

