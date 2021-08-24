{-# LANGUAGE InstanceSigs #-}

module State where

newtype Moi s a = Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \state -> let (a, newstate) = g state
                                     in  (f a, newstate)


instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (<*>) (Moi sab) (Moi sa) = Moi $ \state -> let (ab, _) = sab state
                                                   (a, ns) = sa state
                                               in (ab a, ns)


instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (>>=) (Moi f) g = Moi $ \s -> let (a, ns) = f s
                                  in  runMoi (g a) ns 