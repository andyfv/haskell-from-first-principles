module Test where

import Data.Monoid
import Test.QuickCheck
import Control.Monad

import Bull
import MaybeMonoid

import MonoidLaws


main :: IO ()
main = do
    -- Bull
    quickCheck (monoidAsc  :: BullMappend)
    quickCheck (monoidLeftIdentity  :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)

    -- First'
    quickCheck (monoidAsc  :: FirstMappend)
    quickCheck (monoidLeftIdentity  :: FstId)
    quickCheck (monoidRightIdentity ::  FstId)
