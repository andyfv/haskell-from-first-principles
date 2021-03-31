module Records where

data Person =
    Person { name :: String
           , age :: Int 
           } deriving (Eq, Show)



data FictionType = Fiction deriving Show
data NonfictionType = Nonfiction deriving Show

data BookType 
    = FictionBook FictionType
    | NonfictionBook NonfictionType
    deriving Show

-- 

type AuthorName = String
data Author = Author (AuthorName, BookType)



