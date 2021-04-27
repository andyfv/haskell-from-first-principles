module BindingAndSequencing where

import Control.Applicative ((*>))
import Control.Monad (join)

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' = 
    putStrLn "blah" >>
    putStrLn "another thing"
-- (>>) :: Monad m =>       m a       ->        m b       -> m b
-- (>>) ::                 IO ()      ->       IO ()      -> IO ()
-- (>>)               (putStrLn "hi")    (putStrLn "you")



sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *> 
    putStrLn "another thing"
-- (*>) :: Applicative f =>        f a      ->       f b        -> f b
-- (*>) ::                        IO ()     ->       IO ()      -> IO ()
-- (*>)                     (putStrLn "hi")    (putStrLn "you")


binding :: IO ()
binding = do 
    name <- getLine
    putStrLn name


binding' :: IO ()
binding' = 
    getLine >>= putStrLn  -- passing the value from getLine to putStrLn
-- (>>=) :: Monad m =>     m a   ->   (a    ->  m b)  ->  m b
-- (>>=) ::            IO String -> (String -> IO ()) -> IO ()
-- (>>=)                getLine  ->      putStrLn     -> IO ()


fmapNotEnough :: IO (IO ())
fmapNotEnough =     putStrLn      <$>       getLine
-- (<$>) :: Functor f =>      (a -> b)     ->  f a      -> f b
-- (<$>) :: Functor f => (String -> IO ()) -> IO String -> IO ()
-- (<$>)                     putStrLn      ->  getLine  -> IO ()


fmapWithJoin :: IO ()
fmapWithJoin = join $ putStrLn <$> getLine
-- join :: Monad m =>         m (m a)        -> m a
-- join :: Monad m =>        IO (IO ())      -> IO ()
-- join               (putStrLn <$> getLine) -> IO ()



-- Binding and Sequencing

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO ()
bindingAndSequencing' = 
    putStrLn "name pls:" >>
    getLine >>=
        \name ->
            putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: "
             ++ name ++ " who is "
             ++ age  ++ " years old."
             )


twoBinds' :: IO ()
twoBinds' = 
    putStrLn "name pls:" >>
    getLine >>=
        \name ->
            putStrLn "age pls:" >>
            getLine >>=
                \age ->
                    putStrLn ("y helo thar: "
                             ++ name ++ " who is: "
                             ++ age  ++ " years old."
                             )