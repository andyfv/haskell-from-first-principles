module QuickChecking where

import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)


functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
    (fmap (g . f) x) == (fmap g . fmap f $ x)


-- Test Identity
idTest :: [Int] -> Bool
idTest x = functorIdentity x


-- Test Composition
compTest = functorCompose (+1) (*2)
compTest' x = compTest (x :: [Int])


-- Test Composition
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt ->  IntToInt -> Bool

fc' = functorCompose'


main :: IO ()
main = do
    putStrLn "Test Identity: "
    quickCheck idTest

    putStrLn "Test Composition: "
    quickCheck compTest'

    putStrLn "Test Function Composition"
    quickCheck (fc' :: IntFC)