import ChapterExercises 
    ( Nope
    , PhhhbbtttEither 
    , Identity
    , List
    )
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


type III = (Int, Int, Int)

main :: IO ()
main = do
    quickBatch $ functor (undefined :: Nope III)
    quickBatch $ applicative (undefined :: Nope III)
    quickBatch $ monad (undefined :: Nope III)

    quickBatch $ functor (undefined :: PhhhbbtttEither III III)
    quickBatch $ applicative (undefined :: PhhhbbtttEither III III)
    quickBatch $ monad (undefined :: PhhhbbtttEither III III)

    quickBatch $ functor (undefined :: Identity III)
    quickBatch $ applicative (undefined :: Identity III)
    quickBatch $ monad (undefined :: Identity III)

    quickBatch $ functor (undefined :: List III)
    quickBatch $ applicative (undefined :: List III)
    quickBatch $ monad (undefined :: List III)