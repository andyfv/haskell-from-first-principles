{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Examples (Reader(..), Person(..), Dog(..), asks)

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader ra) (aRb) = Reader $ \r -> runReader (aRb (ra r)) r


getDogRM' :: Reader Person Dog
getDogRM' = do
    name <- asks dogName
    addy <- asks address
    return $ Dog name addy