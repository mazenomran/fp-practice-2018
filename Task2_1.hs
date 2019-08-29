module Task2_1 where

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =    EmptyTree 
                    | Leaf Integer v
                    | Node Integer v (TreeMap v) (TreeMap v)
                    deriving (Show, Eq)


-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Leaf key v) if k == key then True
                      else False
contains (Node key _ l r) k
  | k == key = True
  | k < key  = contains l k
  | k > key  = contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTree = error "Not Found"
lookup (Leaf key v) k = if k == key then v
                      else error "Not Found"
lookup k (Node key v l r)
  | k < key  = lookup k l
  | k > key  = lookup k r
  | k == key = v
  | otherwise = error "Not Found"

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Node k v EmptyTree EmptyTree
insert (k, v) (Leaf key val) = if k == key then Leaf (k v)
                              else   if k < key then (Node key val (insert (k,v) lt) rt)
                                       else Node key val lt (insert (k,v) rt)
                              
insert (k, v) (Node key value l r)
    | k < key   = Node key value (insert (k, v) l) r
    | k > key   = Node key value l (insert (k, v) r)
    | otherwise = (Node key value l r)


-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = EmptyTree
remove i (Leaf key v)  if i == key then EmptyTree
                        else  Leaf key v           
                         
remove i (Node key value l r)
  | i < key = Node key v (remove i l) r
  | i > key = Node key v l (remove i r)
  | otherwise = Node i v EmptyTree EmptyTree

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i EmptyTree = error "error: key not found"
nearestLE i (Node key value left right)
    | i == key = (key, value)
    | i < key  = nearestLE i left
    | i > key  = case (right) of 
        Node key' value' left' _ | i == key' -> (key', value')
                                 | i > key'  -> nearestLE i right
                                 | i < key'  -> case (left') of 
                                        EmptyTree -> (key, value)
                                        otherwise -> nearestLE i left'
        otherwise -> (key, value) 

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
listFromTree EmptyTree = []
listFromTree t         = inorder t

inorder :: TreeMap v -> [(Integer, v)]
inorder (Node key value left right) = listFromTree left ++ [(key, value)] ++ listFromTree right

preorder :: TreeMap v -> [(Integer, v)]
preorder (Node key value left right) = [(key, value)] ++ listFromTree left ++ listFromTree right

postorder :: TreeMap v -> [(Integer, v)]
postorder (Node key value left right) = listFromTree left ++ listFromTree right ++ [(key, value)]

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

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = todo

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
