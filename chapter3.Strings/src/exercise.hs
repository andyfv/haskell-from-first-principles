module Exercise where

dropFirst :: String -> String
dropFirst str = drop 1 str


addLast :: String -> String
addLast str = str ++ "!"

takeFourth :: String -> String
takeFourth str = (str !! 4) : []

dropNine :: String -> String
dropNine str = drop 9 str

takeThird :: String -> String
takeThird str = (str !! 2) : []

letterIndex :: Int -> String
letterIndex pos = ("Curry is awesome!" !! pos) : []

rvrs :: String -> String
rvrs str = let word1 = take 5 str
               word2 = take 2 (drop 6 str)
               word3 = take 7 (drop 9 str)
               in word3 ++ " " ++ word2 ++ " " ++ word1

main :: IO ()
main = do 
        putStrLn (dropFirst testStr)
        putStrLn (addLast testStr)
        putStrLn (takeFourth testStr)
        putStrLn (dropNine testStr)
        where testStr = "Curry is awesome!"

