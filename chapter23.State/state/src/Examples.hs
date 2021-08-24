module Examples where

newtype State  s a = State  { runState  :: s -> (a, s)}
newtype Reader r a = Reader { runReader :: r -> a }


type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- State :: (s -> (a, s)) -> State s a     -- takes an input state and returns an output value 'a'
                                        -- tupled with the new state value
-- runState :: State s a -> s -> (a, s) 


-- random :: (Random a) => StdGen -> (a, StdGen)
-- State {     runState :: s      -> (a, s)}