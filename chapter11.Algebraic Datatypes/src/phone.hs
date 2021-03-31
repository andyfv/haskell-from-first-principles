{- 
    First attempt. Didn't know what to do and sadly left it.
-}

module Phone where

import Data.Char

data Button  = Button Digit [Char] deriving (Show)
type Digit   = Char
type Presses = Int

data Phone = Phone [Button] deriving (Show)

phone :: Phone 
phone = Phone
    [ Button '1' ['1']
    , Button '2' ['a' , 'b' , 'c' , '2']
    , Button '3' ['d' , 'e' , 'f' , '3']
    , Button '4' ['g' , 'h' , 'i' , '4']
    , Button '5' ['j' , 'k' , 'l' , '5']
    , Button '6' ['m' , 'n' , 'o' , '6']
    , Button '7' ['p' , 'q' , 'r' , 's', '7']
    , Button '8' ['t' , 'u' , 'v' , '8']
    , Button '9' ['w' , 'x' , 'y' , 'z' , '9']
    , Button '*' ['*' , ' ']
    , Button '0' ['0' , '+' , ' ']
    , Button '#' ['.' , ',']
    ]

fromCharToButton :: Char -> Button
fromCharToButton c =