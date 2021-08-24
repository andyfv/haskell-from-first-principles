module Exercises where


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, ns) = g s
                                     in  (f a, ns)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)

    (<*>) (State sab) (State sa) = State $ \s -> let (a, _)   = sa s
                                                     (ab, ns) = sab s
                                                 in (ab a, ns)


instance Monad (State s) where
    return = pure

    (>>=) (State f) g = State $ \s -> let (a, ns) = f s
                                      in  runState (g a) ns


get :: State s s 
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)