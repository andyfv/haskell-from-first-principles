module Palindrome where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)    -- Exit successfully, without errors


palindrome :: IO ()
palindrome = forever $ do
    input <- getLine >>= filterInput
    case (input == reverse input) of
        True  -> putStrLn "It's a palindrome!"
        False -> exitSuccess

filterInput :: String -> IO String
filterInput = 
    return 
    . concat 
    . words 
    . filter (not . isPunctuation) 
    . map toLower 
