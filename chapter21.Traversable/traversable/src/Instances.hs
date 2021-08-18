module Instances where

import Prelude hiding (Either(..))

-- 1. Either
data Either a b = Left a | Right b
                  deriving (Eq, Show)

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)


instance Applicative (Either a) where
    pure = Right
    
    (<*>) (Left e) _  = Left e
    (<*>) (Right f) r = fmap f r


instance Foldable (Either a) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _)  = z
    foldr f z (Right y) = f y z

instance Traversable (Either a) where
    traverse _ (Left x)  = pure (Left x)
    traverse f (Right y) = Right <$> f y



-- 2. Tuple
-- instance Functor ((,) a) where
--     fmap f (x,y) = (x, f y)


-- instance Monoid a => Applicative ((,) a) where
--     pure x = (mempty, x)

--     (<*>) (u, f) (v, x) = (u <> v, f x)

    
-- instance Foldable ((,) a) where
--     foldMap f (_, y) = f y
    
--     foldr f z (_, y) = f y z


-- instance Traversable ((,) a) where
--     traverse f (x, y) = (,) x <$> f y