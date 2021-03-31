module IntergralDivision where

type Numerator   = Integer
type Denominator = Integer
type Quotient    = Integer
type Reminder    = Integer

data DevideResult = Result Integer | DevidedByZero deriving Show

devidedBy :: Numerator -> Denominator -> DevideResult
devidedBy num denom =
    go num denom 0 1
    where 
        go n d count sign
            | d == 0 = DevidedByZero
            | d < 0 && n < 0 || d > 0 && n > 0 =
                go (n - d) d (count + 1) (1)
            | n < 0 || d < 0 = go ((abs n) - (abs d)) (abs d) (count + 1) (-1)
            | otherwise = Result (count * sign)