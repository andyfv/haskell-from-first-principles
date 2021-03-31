module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [char]] -> Char
replaceWithP' = replaceWithP

liftedRaplace :: Functor f => f a -> f Char
liftedRaplace = fmap replaceWithP

liftedRaplace' :: [Maybe [Char]] -> [Char]
liftedRaplace' = liftedRaplace


-- Lifted twice
twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap. fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted


-- Lifted thrice
thriceLifted :: (Functor f1, Functor f2, Functor f3) 
             => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted


main :: IO ()
main = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)

    putStr "liftedReplace lms: "
    print (liftedRaplace lms)

    putStr "liftedRaplace' lms: "
    print (liftedRaplace' lms)

    putStr "twiceLifted lms: "
    print (twiceLifted lms)

    putStr "thriceLifted lms: "
    print (thriceLifted lms)

    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)


-- Exercises
-- 1. 
a = fmap (+1) $ read "[1]" :: [Int]


-- 2. 
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = (*2) . (\x -> x - 2)

-- 4. 
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

-- 5. 
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed