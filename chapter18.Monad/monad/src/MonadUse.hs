module MonadUse where


-- List Monad
-- (>>=) :: Monad m =>  m a -> (a ->  m b) ->  m b
-- (>>=) ::            [] a -> (a -> [] b) -> [] b

-- return :: Monad m => a ->  m a
-- return :: Monad m => a -> [] a


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs 
    if even x 
    then [x*x, x*x]
    else [x*x]


twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []



-- Maybe Monad
-- (>>=) :: Monad m =>   m   a -> (a ->   m   b) -> m b
-- (>>=) ::            Maybe a -> (a -> Maybe b) -> Maybe b

-- return :: Monad m => a ->   m   a
-- return :: Monad m => a -> Maybe a


data Cow = Cow { name   :: String
               , age    :: Int 
               , weight :: Int
               } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c = 
    let w = weight c
        n = name c
    in  if (n == "Bess" && w > 499)
        then Nothing
        else Just c 

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of 
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey -> 
                    case noNegative weight' of 
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do 
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)



mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = 
    noEmpty name' >>=
        \nammy -> 
            noNegative age' >>=
                \agey ->
                    noNegative weight' >>=
                        \weighty ->
                            weightCheck (Cow nammy agey weighty)




-- Trying with Applicative
doSomething :: Integer -> (Maybe Integer, Maybe Integer, Maybe String)
doSomething = do
    a <- f
    b <- g
    c <- h
    pure (a,b,c)


doSomething' :: Integer -> Maybe (Integer, Integer, String)
doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)

ab a = putStrLn $ "Hello" ++ a

doSomething''' :: Integer -> (Maybe Integer, Maybe Integer, Maybe String)
doSomething''' = (,,) <$> f <*> g <*> h


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n


g :: Integer -> Maybe Integer
g i = if even i 
        then Just (i + 1)
        else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)


