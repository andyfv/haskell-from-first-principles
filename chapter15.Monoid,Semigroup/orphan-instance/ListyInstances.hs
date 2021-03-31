module ListyInstances where

import Data.Monoid
import Listy


instance Semigroup a => Semigroup (Listy a) where
    (<>) (Listy []) (Listy [])   = Listy []
    (<>) (Listy []) (Listy [l])  = Listy [l]
    (<>) (Listy [l]) (Listy [])  = Listy [l]
    (<>) (Listy [l]) (Listy [l']) = Listy [l <> l']

instance Monoid a => Monoid (Listy a) where
    mempty = Listy []
    
    mappend (Listy l) (Listy l') = Listy $ mappend l l'
