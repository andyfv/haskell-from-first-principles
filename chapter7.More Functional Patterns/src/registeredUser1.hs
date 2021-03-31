module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber String

data User 
    = UnregisteredUser 
    | RegisteredUser Username AccountNumber

