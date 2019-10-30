module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons xs x) = x : rlistToList xs 


listToRList :: [a] -> ReverseList a
listToRList [] = RNil 
listToRList list = foldl (\ revList elem -> RCons revList elem) RNil list

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor


instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _    = False
    (==) _ RNil    = False
    (==) argl argr = rlistToList argl == rlistToList argr

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil     = True
    (<=) RNil _        = True
    (<=) _ RNil        = False
    (<=) argl argr = rlistToList argl <= rlistToList argr

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil end) = show end
    show (RCons start end) = show start ++ "," ++ show end

instance Semigroup (ReverseList a) where
    (<>) = mappend

instance Monoid (ReverseList a) where
    mempty = RNil    
    mappend RNil arg = arg
    mappend arg RNil = arg
    mappend argl argr =
        foldl (\ revList elem -> RCons revList elem) argl (rlistToList argr)

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap arg (RCons start end) = RCons (fmap arg start) (arg end)
