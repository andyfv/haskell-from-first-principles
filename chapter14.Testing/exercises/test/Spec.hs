module Main where

import qualified WordNumberTest as WN
import qualified Exercises as E
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonZero, NonNegative)
import Test.Hspec
import Data.List (sort)


main :: IO ()
main = do
    WN.main


runQc :: IO ()
runQc = hspec $ do
    describe "Using QuickCheck" $ do
        it "half" $
            quickCheck prop_halfDouble
        it "listOrdered" $ do
            quickCheck prop_listOrdered
        
        -- Addition
        it "Addition Associative" $ do
            quickCheck prop_plusAssociative
        it "Addition Commutative" $ do
            quickCheck prop_plusCommutative
        
        -- Multiplication
        it "Multiplication Distributive" $ do
            quickCheck prop_multDistributive
        it "Multiplication Commutative" $ do
            quickCheck prop_multCommutative
        it "Multiplication Associative" $ do
            quickCheck prop_multAssociative
        it "Multiplication Identity" $ do
            quickCheck prop_multIdentity

        -- quotRem 
        it "Quot and Rem" $ do
            quickCheck prop_quotRem
        
        -- divMod 
        it "Div and Mod" $ do
            quickCheck prop_divMod
        
        -- Power 
        it "Power Associative" $ do
            quickCheck prop_powerAssociative
        it "Power Commutative" $ do
            quickCheck prop_powerCommutative

        -- List
        it "twice reversed list should be equal to id" $ do
            quickCheck prop_listReverseTwice

        -- ($)
        it "($)" $ do
            quickCheck prop_dollar

        -- (.)                
        it "(.)" $ do
            quickCheck prop_dot

        -- foldr (:) == (++)
        it "foldr (:) == (++)" $ do
            quickCheck prop_foldrConsWithPlusPlus

        -- foldr (++) == concat
        it "foldr (++) == concat" $ do
            quickCheck prop_foldrPlusPlusWithConcat

        -- read show
        it "read (show x) is equal to x" $ do
            quickCheck prop_readShow

        -- squareIdentity
        it "square of sqrt of x is equal to x" $ do
            quickCheck prop_squareIdentity

        -- Idempotence
        it "function applied two times is equal to \
            \a function applied four times" $ do
                quickCheck prop_idempotence

        it "sorted list is not changed even sorted two or four times" $ do
            quickCheck prop_idempotence'



-- 1
prop_halfDouble :: Property 
prop_halfDouble =
    forAll (arbitrary :: Gen Double) (\x -> E.halfIdentity x == x)


-- 2
prop_listOrdered :: Property
prop_listOrdered = 
    forAll (arbitrary :: Gen [String]) (E.listOrdered . sort)

-- 3.1. Addition associative prop
prop_plusAssociative :: Property 
prop_plusAssociative =
    forAll (arbitrary :: Gen Int) E.plusAssociative

-- 3.2. Addition commutative prop
prop_plusCommutative :: Property 
prop_plusCommutative =
    forAll (arbitrary :: Gen Int) E.plusCommutative

-- 4.1. Multiplication Distributive prop
prop_multDistributive :: Property 
prop_multDistributive =
    forAll (arbitrary :: Gen Int) E.multDistributive

-- 4.2. Multiplication Commutative prop
prop_multCommutative :: Property 
prop_multCommutative =
    forAll (arbitrary :: Gen Int) E.multCommutative


-- 4.3. Multiplication Associative prop
prop_multAssociative :: Property 
prop_multAssociative =
    forAll (arbitrary :: Gen Int) E.multAssociative


-- 4.4. Multiplication Identity prop
prop_multIdentity :: Property 
prop_multIdentity =
    forAll (arbitrary :: Gen Int) E.multIdentity


-- 5. quotRem and divMode helper functions
quotRem' :: Int -> NonZero Int -> Bool
quotRem' x (NonZero y) = E.quotRem x y

divMod' :: Int -> NonZero Int -> Bool
divMod' x (NonZero y) = E.divMod x y


-- 5.1. quotRem prop
prop_quotRem :: Property 
prop_quotRem = 
    forAll (arbitrary :: Gen Int) quotRem'


-- 5.2. divMod prop
prop_divMod :: Property 
prop_divMod = 
    forAll (arbitrary :: Gen Int) divMod'


-- 6.1. Power : associative
prop_powerAssociative :: Property
prop_powerAssociative =
    forAll (arbitrary :: Gen Int) E.powerAssociative


-- 6.2. Power : associative
prop_powerCommutative :: Property
prop_powerCommutative =
    forAll (arbitrary :: Gen Int) E.powerCommutative


-- 7. List 
prop_listReverseTwice :: Property 
prop_listReverseTwice = 
    forAll (arbitrary :: Gen [Char]) E.reverseListTwice


-- 8.1. Function Application ($)
prop_dollarAdd :: Int -> Bool
prop_dollarAdd = E.dollar (+1)


prop_dollar :: Property
prop_dollar = 
    forAll (arbitrary :: Gen Int) prop_dollarAdd


-- 8.2. Function Composition (.)
prop_dotAdd :: Int -> Bool
prop_dotAdd = E.dot (negate) (\n -> negate n)


prop_dot :: Property 
prop_dot =
    forAll (arbitrary :: Gen Int) prop_dotAdd


-- 9.1. foldr (:) == (++)
prop_foldrConsWithPlusPlus :: Property
prop_foldrConsWithPlusPlus =
    forAll (arbitrary :: Gen [Int]) E.foldrConsWithPlusPlus

-- 9.2. foldr (++) == concat
prop_foldrPlusPlusWithConcat :: Property
prop_foldrPlusPlusWithConcat =
    forAll (arbitrary :: Gen [[Int]]) E.foldrPlusPlusWithConcat


-- 10.
fWrap :: (NonNegative Int) -> [Int] -> Bool
fWrap (NonNegative x) ys = E.f x ys

prop_fLength :: Property 
prop_fLength =
    forAll (arbitrary :: Gen (NonNegative Int)) fWrap


-- 11. read show
prop_readShow :: Property
prop_readShow = 
    forAll (arbitrary :: Gen Int) E.readShow


-- Failure
prop_squareIdentity :: Property
prop_squareIdentity = 
    forAll (arbitrary :: Gen Double) E.squareIdentity


-- Idempotence 
prop_idempotence :: Property 
prop_idempotence =
    forAll (arbitrary :: Gen String) E.idempotence


prop_idempotence' :: Property 
prop_idempotence' =
    forAll (arbitrary :: Gen [Int]) E.idempotence'