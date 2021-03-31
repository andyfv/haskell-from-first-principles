module VigenereIO where

import VigenereCipher (vigenere)

main :: IO ()
main = do
    putStrLn "Enter a word to encrypt: "
    word <- getLine
    putStrLn "Enter an encryption word: "
    encryptionWord <- getLine
    let result = vigenere word encryptionWord
    putStrLn $ "The encrypted word is : " ++ result