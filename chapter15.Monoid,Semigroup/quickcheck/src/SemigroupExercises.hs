module SemigroupExercises where

import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = do
    -- Trivial
    quickCheck (semigroupAssoc :: TrivAssoc)

    -- Identity a
    quickCheck (semigroupAssoc :: IdentityAssoc String)

    -- Two a b
    quickCheck (semigroupAssoc :: TwoAssoc [Int] String)

    -- BoolConj
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    
    -- BoolDisj
    quickCheck (semigroupAssoc :: BoolDisjAssoc)

    -- Combine a b
    quickCheck (combAssoc :: CombineAssoc String [Int])

    -- Comp a b
    quickCheck (compAssoc :: CompAssoc [Int])


-- 1. Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


-- 2. Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity a') = Identity (a <> a') 

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do Identity <$> arbitrary 

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool


-- 3. Two a b
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary 
        return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool


-- 4. BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj True) (BoolConj True) = BoolConj True 
    (<>) _ _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    
    mappend (BoolConj True) (BoolConj True) = BoolConj True
    mappend _ _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do BoolConj <$> arbitrary 

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


-- 5. BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
    (<>) _ _                               = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False

    mappend (BoolDisj False) (BoolDisj False) = BoolDisj False
    mappend _ _                               = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = do BoolDisj <$> arbitrary 

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- 6. Combine a b
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Show (Combine a b) where
    show (Combine f) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)
    
instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)

    mappend (Combine f) (Combine g) = Combine (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do Combine <$> arbitrary  

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

combAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combAssoc f g h x =
    unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x


-- 7. Comp a
newtype Comp a = Comp {unComp :: (a -> a)}

instance Show (Comp a) where
    show (Comp a) = "Comp"

instance Semigroup a => Semigroup (Comp a) where
    (<>) (Comp f) (Comp g) = Comp (\x -> f x <> g x)
    
instance Monoid a => Monoid (Comp a) where
    mempty = Comp id 

    mappend (Comp f) (Comp g) = Comp (\x -> f x <> g x)

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = do Comp <$> arbitrary

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

compAssoc :: (Semigroup a , Eq a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h a = 
    unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

-- 8. Mem s a
newtype Mem s a = Mem {runMem :: s -> (a, s)}


instance (Semigroup a) => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem $ 
        \s -> 
            let (a, b) = f s 
                (a', b') = g b
            in (a <> a', b')

instance (Monoid a) => Monoid (Mem s a) where
    mempty  = Mem $ \s -> (mempty, s)
    mappend (Mem f) (Mem g) = Mem $ 
        \s ->
            let (a, b)  = f s
                (a',b') = g b
            in (a `mappend` a', b')