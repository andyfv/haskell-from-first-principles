module TypeclassInstance where

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True
    Trivial' /= Trivial' = False 


-- Days and Date Types
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

data Date = Date DayOfWeek Int deriving Show


instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False


instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') = 
        weekday == weekday' && dayOfMonth == dayOfMonth'

-- Identity Type
data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'