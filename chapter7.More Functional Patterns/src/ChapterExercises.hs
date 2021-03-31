module ChapterExercises where

tensDigit :: Integral a => a -> (a, a)
tensDigit x = x `divMod` 10

hundreds :: Integral a => a -> (a, a)
hundreds x = x `divMod` 100


-- 2

foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b == True  = y
    | otherwise  = x

-- 3 
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)