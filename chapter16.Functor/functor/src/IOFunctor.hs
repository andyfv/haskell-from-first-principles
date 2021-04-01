module IOFunctor where

getInt :: IO Int
getInt = fmap read getLine


-- Ignore the input and print nothing - ()
getInt' :: IO ()
getInt' = fmap (const ()) getLine



-- using `do syntax` instaed of fmap. 
meTooIsm :: IO String 
meTooIsm = do
    input <- getLine 
    return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
    intVal <- getInt
    return (intVal + 1)

