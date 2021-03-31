module EitherLibrary where


-- 1. Lefts
lefts' :: [Either a b] -> [a]
lefts' = foldr go ([])
    where go (Left x) acc  = x:acc
          go _ acc         = acc


-- 1. Rights
rights' :: [Either a b] -> [b]
rights' = foldr go ([])
    where go (Right x) acc  = x:acc
          go _ acc          = acc


-- 3. Parition Either
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([],[])
    where go (Left a)  (lAcc, rAcc) = (a:lAcc, rAcc)
          go (Right b) (lAcc, rAcc) = (lAcc, b:rAcc)


-- 4. Either to Maybe
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _         = Nothing


-- 5. Catamorphism for Either
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a)  = aToC a
either' _ bToC (Right b) = bToC b


-- 6. 
-- eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
-- eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
