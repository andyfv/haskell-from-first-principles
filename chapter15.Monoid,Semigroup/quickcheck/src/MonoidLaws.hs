module MonoidLaws where

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
    a <> (b <> c) == (a <> b) <> c

monoidAsc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAsc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a