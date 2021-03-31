module CipherIO where

import Cipher (encrypt)

main :: IO ()
main = do
    putStrLn "Enter word to encrypt: "
    word <- getLine
    putStrLn "Enter number of shifts: "
    shiftString <- getLine
    let shift  = (read shiftString :: Int)
    let result = encrypt shift word
    putStr $ "The encrypted word is: " ++ result
    return ()
