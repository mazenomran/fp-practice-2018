module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =    EmptyTree 
                    | Leaf Integer v
                    | Node Integer v (TreeMap v) (TreeMap v) deriving(Show) 
                    
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Leaf key v) k = key == k
contains (Node key v lt rt) k | k < key = contains lt k
                              | k > key = contains rt k
                              | otherwise = True

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTree = error "The tree is empty."

lookup k (Leaf key v)  | k == key = v
                       | otherwise = error "No such element in leaf."                                

lookup k (Node key v lt rt) | k < key = lookup k lt
                            | k > key = lookup k rt
                            | otherwise = v

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Leaf key val)        
                        | k < key = Node key val (Leaf k v) (EmptyTree)
                        | k > key = Node key val (EmptyTree) (Leaf k v)
                        | otherwise = Leaf k v

insert (k, v) (Node key val lt rt)  
                        | k < key = (Node key val (insert (k,v) lt) rt)
                        | k > key = (Node key val lt (insert (k,v) rt))
                        | otherwise = Node k v lt rt

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = EmptyTree
remove i (Leaf key v)           
                        | i == key = EmptyTree
                        | otherwise = Leaf key v       
                                
remove i (Node key v EmptyTree (Leaf key' v'))      
                        | i == key = Leaf key' v'
                        | i == key' = Leaf key v
                        | otherwise = Node key v EmptyTree (Leaf key' v') 

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = todo

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = todo

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTree = error "error: key not found"
kMean i (Node key value left right)
    | i == (size left) = (key, value)
    | i < (size left)  = kMean i left
    | i > (size left)  = kMean (i - size left - 1) right

size :: TreeMap v -> Integer
size EmptyTree             = 0
size (Node _ _ left right) = size left + 1 + size right
