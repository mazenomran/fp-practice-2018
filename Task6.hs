module Task6 where

import Todo(todo)


-- a binary search tree
data LinkedTree a = Leaf { parent :: LinkedTree a }
  | Node { value :: a -- payload
         , left :: LinkedTree a -- left subtree
         , right :: LinkedTree a -- right subtree
         , parent :: LinkedTree a -- parent subtree
         }

-- searches for an element in a search tree
find :: Ord a => a -> LinkedTree a -> Maybe (LinkedTree a)
find _ (Leaf _) = Nothing
find v' n@(Node v l r _) =
  case compare v' v of
    EQ -> Just n
    LT -> find v' l
    GT -> find v' r
-- inserts an element into a binary search tree
insert :: Ord a => a -> LinkedTree a -> LinkedTree a
insert v' (Leaf parent) = 
  let result = Node v' (Leaf result) (Leaf result) parent
  in result
insert v' n@(Node v l r p) = 
  case compare v' v of
    EQ -> n
    LT -> let inserted = insert v' l
              result = Node v inserted r p
          in result
    GT -> let inserted = insert v' r
              result = Node v l inserted p
          in result

-- an examplary tree for testing purposes
{-
     5
    /  \
  3     8 
 / \   / \
1   4 6   10
 \     \  /
  2     7 9
-}

remove :: LinkedTree a -> a -> LinkedTree a
remove = todo
