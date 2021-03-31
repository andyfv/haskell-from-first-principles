module NaturalNumbers where

{- 
    Valid Natural numbers: from 0 to infinity

    (Integer (Natural)) Integers is a superset of Natural
        - Any Natural can be represented by an Integer
        - Negative Integers can not be represented by Naturals

    Implement functions to:
        - convert Naturals to Integers (Nat -> Integer)
        - convert Integers to Naturals (Integer -> Nat)
-}
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Zero)   = 0
natToInteger (Succ n) = 1 + natToInteger n


integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0     = Nothing
    | otherwise = Just $ mkNat n
    where mkNat 0 = Zero
          mkNat i = Succ (mkNat $ i - 1)