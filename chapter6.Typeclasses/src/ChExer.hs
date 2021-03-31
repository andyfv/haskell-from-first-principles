module ChExer where

import Data.List

-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving Show

instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _       = False


instance Ord Mood where
    (>) Woot Blah     = True
    (>) Blah Woot     = False
    (>) _ _           = False
    compare Woot Blah = GT
    compare Blah Woot = LT
    compare Woot Woot = EQ
    compare Blah Blah = EQ


settleDown :: Mood -> Mood
settleDown x = if x == Woot
                then Blah
                else x


-- 3
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving Show

-- Does not Typecheck
-- s1 :: Sentence
-- s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"


-- 4

data Rocks = Rocks String deriving (Eq, Show)
data Yeah  = Yeah Bool deriving (Eq, Show)
data Papu  = Papu Rocks Yeah deriving (Eq, Show)

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus ::  Papu -> Papu -> Bool
-- comparePapus p p' = p > p'


-- Match the Types
i :: Num a => a
i = 1

f :: RealFrac a => a 
f = 1.0

freud :: Ord a => a -> a
freud x = x

-- 
myX = 1

sigmund :: Int -> Int
sigmund x = myX


-- 
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)



-- 

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (xs)


-- 

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- Types

-- 1 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = (aTob a) == b

-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB n a = (fromInteger n) + (aToB a)
