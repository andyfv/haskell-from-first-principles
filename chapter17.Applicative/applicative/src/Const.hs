module Const where

{-
    f ~ Constant e
    type C = Constant

    (<*>) ::   f (a -> b) ->   f a ->   f b
    (<*>) :: C e (a -> b) -> C e a -> C e b

    pure :: a ->   f a
    pure :: a -> C e a
-}

newtype Constant a b = Constant {getConstant :: a}
                        deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant a') = Constant (a <> a')

    