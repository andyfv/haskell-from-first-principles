{-# LANGUAGE FlexibleInstances #-}
module ChapterExercies where

import GHC.Arr


-- 1. Bool
-- data Bool = False | True     -- not valid

-- 2. BoolAndSomethingElse a
data BoolAndSomethingElse a = False' a | True' a 

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a)  = True' (f a)


-- 3. BoolAndMaybeSomethingElse a 
data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
    fmap f (Falsish)  = Falsish
    fmap f (Truish a) = Truish (f a)


-- 4. newtype Mu f 
newtype Mu f = InF {outF :: f (Mu f)}

-- Kind: (* -> *) -> *


-- 5. data D
data D = D (Array Word Word) Int Int    -- All arguments are applied,
                                        -- so this is not valid
-- > :k D
-- D :: *    



---------- Rearange ----------
-- 1. Sum a b
data Sum a b = First b | Second a

instance Functor (Sum e) where
    fmap f (First a)  = First (f a)
    fmap _ (Second b) = Second b


-- 2. Company a b c
data Company a b c = DeepBlue a b | Something c

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


-- 3. More a b
data More a b = L b a b 
              | R a b a 
              deriving (Eq, Show)


instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'




---------- Functor Instances ----------
-- 1. Quant a b
data Quant a b = Finance | Desk  a | Bloor b

instance Functor (Quant a) where
    fmap _ (Finance) = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor a) = Bloor (f a)


-- 2. K a b
data K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a


-- 3. Flip f a b
newtype Flip f a b = Flip (f b a) deriving (Eq, Show) 

newtype K' a b = K' a

instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip $ K' (f a)


-- 4. EvilGoateeConst a b
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)


-- 5. LiftItOut f a
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


-- 6. Parappa f g a 
data Parappa f g a = DaWrapppa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrapppa fa ga) = DaWrapppa (fmap f fa) (fmap f ga)

-- 7. IgnoreOne f g a b 
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = 
        IgnoringSomething fa (fmap f gb)


-- 8. Notorious g o a t
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9. List a 
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a fa) = Cons (f a) (fmap f fa)

-- 10. GoatLord a 
data GoatLord a = NoGoat 
                | OneGoat a 
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat               = NoGoat
    fmap f (OneGoat a)          = OneGoat (f a)
    fmap f (MoreGoats f1 f2 f3) = MoreGoats (fmap f f1)
                                            (fmap f f2)
                                            (fmap f f3)


-- 11. TalkToMe a 
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g)    = Read (f . g)   -- Read ((a -> b) . (String -> a))