module TestFuncInstances where

import Test.Hspec
import Test.QuickCheck

import QuickChecking
import FuncInstances

test = hspec $ do
    describe "Identity a" $ do
        it "Identity" $ property (functorIdentity :: (Identity Int) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Identity Int) 
                                                  -> IntToInt 
                                                  -> IntToInt 
                                                  -> Bool)

    describe "Pair a" $ do
        it "Identity" $ property (functorIdentity :: (Pair Int) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Pair Int) 
                                                  -> IntToInt 
                                                  -> IntToInt 
                                                  -> Bool)

    describe "Two a b" $ do
        it "Identity" $ property (functorIdentity :: (Two Int Char) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Two Int Char) 
                                                  -> Fun Char Int 
                                                  -> Fun Int Int 
                                                  -> Bool)

    describe "Three a b c" $ do
        it "Identity" $ property (functorIdentity :: (Three Char Bool Int) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Three Char Bool Int) 
                                                  -> Fun Int Char 
                                                  -> Fun Char Int 
                                                  -> Bool)

    describe "Three' a b" $ do
        it "Identity" $ property (functorIdentity :: (Three' Int Char) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Three' Int Char) 
                                                  -> Fun Char Int
                                                  -> Fun Int Char
                                                  -> Bool)

    describe "Four a b c d" $ do
        it "Identity" $ property (functorIdentity :: (Four Bool Char Int Bool) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Four Bool Char Int Bool)
                                                  -> Fun Bool Int
                                                  -> Fun Int Bool
                                                  -> Bool)

    describe "Four' a b" $ do
        it "Identity" $ property (functorIdentity :: (Four' Int Char) -> Bool)
        it "Compose"  $ property (functorCompose' :: (Four' Int Char)
                                                  -> Fun Char Int
                                                  -> Fun Int Char
                                                  -> Bool)