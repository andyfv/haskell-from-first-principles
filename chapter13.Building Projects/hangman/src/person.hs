module Person where

type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                    "Name was: " ++ show name ++
                                    " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Enter person name: "
    name <- getLine
    putStrLn "Enter person age: "
    ageStr <- getLine 
    let age = (read ageStr :: Integer)
    case mkPerson name age of
        Right p -> putStrLn $ "Yay! Successfully got a person : "
                                        ++ show p
        Left s  -> putStrLn $ "Error: " ++ show s
                            

