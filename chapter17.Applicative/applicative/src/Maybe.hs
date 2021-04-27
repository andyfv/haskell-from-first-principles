module Maybe where

newtype Name    = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address 
mkAddress a = fmap Address $ validateLength 100 a

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
    if (length s) > maxLen 
        then Nothing 
        else Just s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = 
    case mkName n of
        Nothing -> Nothing 
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing 
                Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a


mkPerson'' :: String -> String -> Maybe Person
mkPerson'' name' address' = do
    n <- mkName name'
    a <- mkAddress address'
    pure (Person n a)

mkPerson''' :: String -> String -> Maybe Person
mkPerson''' name' address' = 
    mkName name' >>= \n -> mkAddress address' >>= \a -> pure (Person n a)



mkTuple :: String -> String -> Maybe (Name, Address)
mkTuple name' address' =
    mkName name' >>= \n -> mkAddress address' >>= \a -> pure ((,) n a)

mkTuple' :: String -> String -> Maybe (Name, Address)
mkTuple' name' address' = do
    n <- mkName name'
    a <- mkAddress address'
    pure ((,) n a)

mkTuple'' :: String -> String -> Maybe (Name, Address)
mkTuple'' name' address' = 
    (,) <$> mkName name' <*> mkAddress address'

-- 
makeSequentialTuple :: String -> Maybe (String, String)
makeSequentialTuple name' = do
    a <- mkName' name'
    b <- mkAddress' a
    pure (a,b)




makeSequentialTuple' :: String -> (Maybe String, Maybe String)
makeSequentialTuple' = do
    a <- mkName'            
    b <- mkAddress'
    pure (a, b)


makeSequentialTuple'' :: String -> (Maybe String, Maybe String)
makeSequentialTuple'' = 
    (,) <$> mkName' <*> mkAddress'



makeSequentialTuple''' :: String -> Maybe (String, String)
makeSequentialTuple''' = 
    \str -> 
        mkName' str
    >>=
    \name' -> 
        mkAddress' name'
    >>=
    \address' -> 
        pure (name', address')

-- 

mkName' :: String -> Maybe String
mkName' n = validateLength 25 n

mkAddress' :: String -> Maybe String
mkAddress' a = validateLength 100 a




-- Triple
mkTriple :: String -> String -> String -> Maybe (Name, Name, Address)
mkTriple fname lname address' = 
    (,,) <$> mkName fname <*> mkName lname <*> mkAddress address'

mkTriple' :: String -> String -> String -> Maybe (Name, Name, Address)
mkTriple' fname lname address' = do
    fname' <- mkName fname 
    lname' <- mkName lname 
    addr   <- mkAddress address'
    pure ((,,) fname' lname' addr)

mkTriple'' :: String -> String -> String -> Maybe (Name, Name, Address)
mkTriple'' fname lname address' = 
    mkName fname >>= \fname' -> mkName lname 
    >>= 
    \lname' -> mkAddress address'
    >>= 
    \address' -> pure ((,,) fname' lname' address')






data Cow = Cow 
    { name   :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)