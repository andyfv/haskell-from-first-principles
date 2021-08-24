module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL


------------------------------ VERSION 1 ------------------------------
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n


-- main :: IO ()
-- main = mapM_ (putStrLn . fizzBuzz) [1 .. 100]


------------------------------ VERSION 2 ------------------------------
fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzList [1 .. 100]


------------------------------ EXERCISE ------------------------------
addResult' :: Integer -> State [String] ()
addResult' n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end = execState (mapM_ addResult' [end, end - 1 .. start]) []
