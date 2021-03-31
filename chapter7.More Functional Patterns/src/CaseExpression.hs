module CaseExpresion where

funcZ x = 
    case x + 1 == 1 of
        True -> "AWESOME"
        False -> "wut"

functionC x y = 
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 n = 
    case even n of
        True -> (n + 2)
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: ( Employee -> Employee -> Ordering ) 
                -> Employee 
                -> Employee 
                -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'


-- 
-- dodgy :: Integer -> Integer -> Integer
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2