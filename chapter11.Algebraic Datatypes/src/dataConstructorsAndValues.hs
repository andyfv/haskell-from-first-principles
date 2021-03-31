module DataConstructorsAndValues where

-- 
data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge


-- Values
myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData


myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int 
myDoge = DogueDeBordeaux 10

-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a
    = Husky a
    | Mastiff a
    deriving (Eq, Show)


data Price      =   Price Integer deriving (Show, Eq)
--     |              |         \
--    type           data        type
-- constructor    constructor   argument

data Size = Size Integer deriving (Show, Eq)

data Manufacturer 
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)


data Vehicle
    = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)


myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

isCar :: Vehicle -> Bool
isCar v =
    case v of
        Car _ _   -> True
        Plane _ _ -> False

isPlane :: Vehicle -> Bool
isPlane v =
    case v of
        Plane _ _ -> True
        Car _ _   -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu v = 
    case v of
        Car manu _ -> Just manu
        _          -> Nothing


data Example = MakeExample deriving Show
data Example2 = MakeExample2 Int deriving Show


-- data Goats = Goats Int deriving (Eq, Show)
newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
    tooMany (Goats n) = n > 43

newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42