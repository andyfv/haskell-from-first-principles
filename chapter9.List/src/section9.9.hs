module Section9_9 where

import Data.Bool

myFunc xs = map (\x -> bool x (-x) (x == 3)) xs