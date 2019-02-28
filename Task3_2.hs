module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil               = []
rlistToList (RCons prev value) = (rlistToList prev) ++ [value]

listToRList :: [a] -> ReverseList a
listToRList []    = RNil
listToRList (h:t) = RCons (listToRList t) h

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil                                 = True
    (==) RNil _                                    = False
    (==) _ RNil                                    = False
    (==) (RCons prev1 value1) (RCons prev2 value2) = prev1 == prev2 && value2 == value2

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil                                 = True
    (<=) RNil _                                    = True
    (<=) _ RNil                                    = False
    (<=) (RCons prev1 value1) (RCons prev2 value2) = value1 <= value2 || prev1 <= prev2

instance (Show a) => Show (ReverseList a) where
    show RNil               = "[]"
    show (RCons RNil value) = show value
    show (RCons prev value) = show value ++ ", " ++ show prev

instance Semigroup (ReverseList a) where
    (<>) = mappend

instance Monoid (ReverseList a) where
    mempty = RNil

    mappend RNil list               = list
    mappend list RNil               = list
    mappend list (RCons prev value) = RCons (mappend list prev) value

    mconcat = foldr mappend mempty

instance Functor ReverseList where
    fmap _ RNil                      = RNil
    fmap function (RCons prev value) = RCons (fmap function prev) (function value)
