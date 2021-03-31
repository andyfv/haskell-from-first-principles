module MaybeMonoid where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada Nada         = Nada
    (<>) Nada (Only a)     = Only a
    (<>) (Only a) Nada     = Only a
    (<>) (Only a) (Only b) = Only (a <> b)


instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend a Nada = a
    mappend Nada b = b
    mappend (Only a) (Only b) = Only (mappend a b)

    mconcat = foldr mappend mempty
