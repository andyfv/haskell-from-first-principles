module Section10_4 where

xs = map show [1..5 ]
y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs