module Task7 where

import Todo(todo)


data Deque a = Deque [a] [a]

-- Пустая очередь
empty :: Deque a
empty = Deque [] []

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Deque [] []) v = Deque [v] []
pushFront (Deque in_ out) v = Deque (v:in_) out

halve :: [a] -> ([a], [a])
halve lst = splitAt (length lst `div` 2) lst

equalize :: Deque a -> Deque a
equalize (Deque in_ []) = Deque (fst half) (reverse $ snd half) where
  half = halve in_
equalize (Deque [] out) = Deque (reverse $ snd half) (fst half) where
  half = halve out

-- Удаление из начала очереди
popFront :: Deque a -> (Deque a, a)
popFront (Deque [] []) = error "Deque is empty"
popFront (Deque in_ (h:t)) = (Deque in_ t, h)

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
-- pushBack (Deque [] []) v = Deque [] (v:[])
-- pushBack (Deque ([a]) []) = (Deque [] [x])
pushBack deque@(Deque in_ []) = pushBack $ equalize deque



-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> ( Deque a, a)
popBack (Deque [] []) = error "Deque is empty"
popBack (Deque [] (h:[])) = (Deque [] [], h)
popBack deque@(Deque [] out) = popBack $ equalize deque

-- Метод банкира:
--   Зарабатываем 1$ за каждое добавление.
--   При удалении из деки может возникнуть случай, когда нужный список опустел.
--   Тогда нужно производить "перекидывание".
--   Перекидываем в этом случае N/2, где N мы уже заработали на добавлениях.
--   Соответственно, амортизированная сложность всех операций над декой - константа.
--
-- Метод физика:
--   Потенциал - сумма длин списков.
--   Добавление увеличивает потенциал на 1.
--   Удаление без "перекидывания" не меняет потенциал.
--   Удаление с "перекидыванием" уменьшает потенциал на N и занимает O(N),
--   но случается не чаще чем каждую N/2 операцию.


