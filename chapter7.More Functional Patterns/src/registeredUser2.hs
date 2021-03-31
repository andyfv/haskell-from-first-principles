module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User
    = UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser user = 
    case user of
        UnregisteredUser -> putStrLn "UnregisteredUser"

        RegisteredUser (Username name) (AccountNumber acctNum) ->
            putStrLn $ name ++ " " ++ show acctNum


data WherePenguisLive 
    = Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguisLive deriving (Eq, Show)

isSouthAfrica :: WherePenguisLive -> Bool
isSouthAfrica place = 
    case place of
        SouthAfrica -> True
        _           -> False

gimmeWhereTheyLive :: Penguin -> WherePenguisLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False


antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p) || (antarcticPenguin p)
