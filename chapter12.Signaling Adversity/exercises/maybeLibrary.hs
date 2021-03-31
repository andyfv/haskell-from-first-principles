module MaybeLibrary where


-- 1. Boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust (Just _)  = True
isJust _         = False

isNothing :: Maybe a -> Bool
isNothing (Nothing) = True
isNothing _         = False


-- 2. Maybe cathamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ (Nothing) = b 
mayybee b f (Just a)  = f a 


-- 3. Fallback
fromMaybe :: a -> Maybe a -> a
fromMaybe a (Nothing) = a
fromMaybe _ (Just a)  = a


-- 4. Converting between List and Maybe 
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe [a]   = Just a 
listToMaybe (a:_) = Just a 

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]


-- 5. Drop Nothing values from List
catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs


-- 6. Flip (Sequence)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
    where go Nothing _ = Nothing
          go _ Nothing = Nothing
          go (Just x) (Just xs) = Just (x:xs)

