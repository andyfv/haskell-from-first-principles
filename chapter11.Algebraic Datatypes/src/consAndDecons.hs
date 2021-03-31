module ConstructionAndDeconstruction where

data Sum a b 
    = First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct 
        { pfirst :: a
        , psecond :: b
        } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig