module ApplicationAndComposition where

import Control.Monad ((>=>), join)


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))


mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f


-- Similarities between flip (.) and (>=>)
flipComp :: (a -> b) -> (b -> c) -> a -> c
flipComp = flip (.)

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flipComp ::         (a ->   b) -> (b ->   c) -> a ->   c


-- Example

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
