module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True   = [True]
eftBool True False  = []  
eftBool False True  = [False, True]


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2
    | o1 > o2   = []
    | o1 == o2  = [o1]
    | otherwise = o1 : (eftOrd (succ o1) o2)


eftInt :: Int -> Int -> [Int]
eftInt start end
    | start > end && end < max     = []
    | start == end && start == max = [start]
    | otherwise                    = start : (eftInt (start + 1) end)
    where max = maxBound :: Int

eftChar :: Char -> Char -> [Char]
eftChar a z 
    | a > z = []
    | a == z = [a]
    | otherwise = a : (eftChar (succ a) z)