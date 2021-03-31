module Bull where

import Test.QuickCheck


data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo)
                          ]

instance Semigroup Bull where
    (<>) Fools Fools = Fools
    (<>) Twoo Fools  = Twoo
    (<>) Fools Twoo  = Twoo
    (<>) Twoo Twoo   = Twoo


instance Monoid Bull where 
    mempty      = Fools
    mappend _ _ = Fools


type BullMappend = Bull -> Bull -> Bull -> Bool