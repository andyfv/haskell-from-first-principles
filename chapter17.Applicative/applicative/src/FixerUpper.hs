module FixerUpper where


-- 1
f :: Maybe [Char]
f = const <$> Just "Hello" <*> pure "World"


-- 2
g :: Maybe (Integer, Integer, [Char], [Integer])
g = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
