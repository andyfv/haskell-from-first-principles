module Addition where

import Test.Hspec
import Test.QuickCheck
import System.Random

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "15 devided by 3 is 5" $ do
            devideBy 15 3 `shouldBe` (5, 0)
        it "22 devided by 2 is 4 reminder 2" $ do 
            devideBy 22 5 `shouldBe` (4, 2)
        it "2 multiplied by 3 is 6" $ do
            mult 2 3 `shouldBe` 6
        it "4 multiplied by 0 is 0" $ do
            mult 4 0 `shouldBe` 0
        it "(-4) multiplied by 1 is (-4)" $ do
            mult (-4) 1 `shouldBe` (-4)
        it "0 multiplied by 1 is 0" $ do
            mult 0 1 `shouldBe` 0 
        -- 
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x ::  Int)


-- QuickCheck
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x


runQc :: IO ()
runQc = quickCheck  prop_additionGreater



-- Hspec
devideBy :: Integral a => a -> a -> (a, a)
devideBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


-- Multiplies two numbers using recursive summation
mult :: (Ord a, Eq a, Num a) => a -> a -> a
mult x y = go x y
    where go a b
            | (a == 0) || (b == 0) = 0
            | a == 1 = b
            | b == 1 = a
            | a < 0  = negate $ go (negate a) b
            | b < 0  = negate $ go a (negate b)
            | otherwise = a + go a (b - 1)


-- 

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1,2,2,2,2,3]

-- genBool :: Bool
-- genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char 
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a , Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)
    

-- Ex. sample (genEither :: Gen (Either Int Char))
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do 
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]


genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))
              ]



