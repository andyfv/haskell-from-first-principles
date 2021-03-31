module MonoidLaws where 

-- Identity
mappend mempty x = x    -- left identity

mappend x mempty = x    -- right identity



-- Associativity
mappend x (mappend y z) = 
    mappend (mappend x y) z

mconcat = foldr mappend mempty



-- mappend infix operator 

(<>) :: Monoid m => m -> m -> m

-- Infix : Associativity
(Sum 1) <> (Sum 2 <> Sum 3)
Sum {getSum = 6}

(Sum 1 <> Sum 2) <> (Sum 3)
Sum {getSum = 6}

mconcat [(Sum 1), (Sum 2), (Sum 3)]
Sum {getSum = 6}

foldr mappend mempty [(Sum 1), (Sum 2), (Sum 3)]
Sum {getSum = 6}



-- Monoid Lists
-- mempty is []
-- mappend is (++)

-- Identity
mappend mempty [1, 2, 3]    -- left identity
mappend [1, 2, 3] mempty    -- right identity


-- Associativity
[1] <> ([2] <> [3])
[1,2,3]

([1] <> [2]) <> [3]
[1,2,3]

-- mconcat ~ foldr mappend mempty
mconcat [[1], [2], [3]]
[1,2,3]

foldr mappend mempty [[1], [2], [3]]
[1,2,3]

concat [[1], [2], [3]]
[1,2,3]
