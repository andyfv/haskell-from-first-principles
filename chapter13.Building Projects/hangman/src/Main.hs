{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever)      -- Infinte loop
import Data.Char (toLower)          
import Data.Maybe (isJust)          
import Data.List (intersperse)
import System.Exit (exitSuccess)    -- Exit successfully, without errors
import System.Random (randomRIO)    


newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)


-- Word lenght bounds
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9


-- Filter allWords with the predefined word length bounds
gameWords :: IO WordList
gameWords = do
    (WordList wl) <- allWords
    return $ WordList (filter gameLength wl)
    where gameLength w =
            let l = length (w :: String)
            in  l >= minWordLength 
            &&  l <  maxWordLength


-- Get a random word
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0 , (length wl) - 1)
    return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- Puzzle

data Puzzle = 
    Puzzle 
    { word         :: String
    , filled       :: [Maybe Char]
    , guessed      :: [Char]
    , attemptsLeft :: Int
    }


instance Show Puzzle where
    show (Puzzle {..}) =
        (intersperse ' ' $ fmap renderPuzzleChar filled)
        ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle 
    { word = word
    , filled = fmap (const Nothing) word
    , guessed = []
    , attemptsLeft = length word
    }


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle {word = word}) c = elem c word


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle {guessed = guessed}) c = elem c guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Nothing) = '_'
renderPuzzleChar (Just c)  = c


fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter p@(Puzzle {..}) c =
    p { filled = newFilled
      , guessed = (c: guessed)
      }
    where newFilled = zipWith (zipper c) word filled
          zipper currGuess wordChar prevGuess = 
              if   wordChar == currGuess
              then Just wordChar
              else prevGuess


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess p@(Puzzle {..}) guess = do
    putStrLn $ "Your guess was : " ++ [guess]
    case (charInWord p guess, alreadyGuessed p guess) of
        (_, True) -> do
            putStrLn "You already guessed that\
                     \ character, pick \
                     \ something else!"
            return p
        
        (True, _) -> do
            putStrLn "This character was in the\
                     \ word, filling in the word\
                     \ accordingly"
            return (fillInCharacter p guess)
        
        (False, _) -> do
            putStrLn "This character wasn't in\
                     \ the word, try again."
            return (fillInCharacter (p {attemptsLeft = attemptsLeft - 1}) guess)

        

gameOver :: Puzzle -> IO ()
gameOver (Puzzle {word = word, attemptsLeft = attemptsLeft}) = 
    if   (attemptsLeft) == 0 
    then 
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ word
           exitSuccess
    else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle {filled = filled}) =
    if all isJust filled
    then
        do putStrLn "You win!"
           exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle 
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle