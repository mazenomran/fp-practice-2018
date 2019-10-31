module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec



insertAt :: DList a -> Int -> a -> DList a
insertAt DNil i item |i < 0 = error "WrongIndex"
                      | i == 0 = DCons DNil item DNil 
                      | i > 0  = error "List is too small"
inserAt (DCons l v r) 0 item = new_node where
                                         new_node = DCons l item right 
                                         right = DCons new_node v r 
inserAt (DCons l v r) item n = DCons l v $ insertAt r item (n-1)




removeAt :: (Eq a) => DList a -> a -> DList a
removeAt  DNil _ = DNil 
removeAt  (DCons l v r@(DCons ll vv rr)) c
                   | v == c = DCons l vv rr
                   | otherwise = DCons l v $ removeAt r c
removeAt  (DCons l v DNil) c = DNil


-- ndx :: DList a -> Int -> a
-- ndx Dlist i |i < 0 = error "WrongIndex"
--             |Dlist == DNil = error "EmptyList"
--             |otherwise ndx' Dlist i 
--       let
--          ndx' (DCons l v r) i | i== 0 = v
--                               |ndx' r (i-1)
