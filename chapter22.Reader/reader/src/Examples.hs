{-# LANGUAGE InstanceSigs #-}

module Examples where

import Control.Applicative 
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

-- fmap boop doop x == (* 2) ((+ 10) x)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


-- >>> bbop 3
-- 19
-- 
-- It feeds the same argument to boop and doop simultaneously
-- (+)  <$> boop    <*> doop
-- ((+) <$> boop    <*> doop) 3
-- ((+) <$> (* 2)   <*> (+ 10)) 3
-- (+)  <$> (* 2) 3 <*> (+ 10) 3
-- (+)  <$> (* 2 3) <*> (+ 10 3)
-- (+)  <$> 6       <*> 13
-- (+)      6           13
-- 19

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)


boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop 
    b <- doop
    return (a + b)

---------------------------------------------------
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tuppled :: [Char] -> ([Char], [Char])
tuppled = (,) <$> cap <*> rev

tuppled' :: [Char] -> ([Char], [Char])
tuppled' = do
    reversed    <- rev
    capitalized <- cap
    return (capitalized, reversed)


-- Ask

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a 
ask = Reader id


-- Applicative for Functions
newtype HumanName = HumanName String
                    deriving (Eq, Show)

newtype DogName = DogName String
                  deriving (Eq, Show)

newtype Address = Address String
                  deriving (Eq, Show)



data Person = Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)


data Dog = Dog
    { dogsName   :: DogName
    , dogAddress :: Address
    } deriving (Eq, Show)


pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")


chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)


-- With Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address


(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

-- Again with Reader but more verbose
getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address


-- One more way
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

--    ^
--    |
-- In the case above we are not threading the argument like a needle 
-- through our functions
-- Instead, we elide it (lift the argument) and let the types manage it for us

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

testMyLift :: Person -> Dog
testMyLift = myLiftA2 Dog dogName address

asks :: (r -> a) -> Reader r a
asks f = Reader f



-------------------------- Reader Applicative --------------------------
instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (fmap f ra)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rab) (Reader ra) = 
        Reader $ \r -> rab r (ra r)
    ------------------- 
    ------------------- rab :: r -> a -> b
    ------------------- ra  :: r -> a
    ------------------- r   :: r
    ------------------- (rab -> r) :: (r -> a -> b) -> r -> (a -> b)
    ------------------- (ra  -> r) :: (r -> a)      -> r -> a
    ------------------- (rab -> r) -> (ra -> r) :: (a -> b) -> a -> b
    -----------------------------------------------------------------



getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy


-------------------------------------------------------------------
-- withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
-- withReaderT f m = ReaderT $ runReaderT m . f