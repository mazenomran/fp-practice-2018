module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Prod deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ operation :: Operation, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|)  l r = l |+| r
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) l r = l |-| r
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) l r = l |*| r
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ (IntConstant intValue) = IntConstant intValue
replaceVar var term (Variable varName) = if varName == var then term else Variable varName
replaceVar var term (BinaryTerm operartion lhv rhv) = BinaryTerm operartion (replaceVar var term lhv ) (replaceVar var term rhv )

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm (Plus) (IntConstant 0) (IntConstant r)) = IntConstant (r) 
evaluate (BinaryTerm (Plus) (IntConstant l) (IntConstant 0)) = IntConstant (l) 
evaluate (BinaryTerm (Plus) (IntConstant l) (IntConstant r)) = IntConstant (l + r)  
evaluate (BinaryTerm (Minus) (IntConstant l) (IntConstant 0)) = IntConstant (l) 
evaluate (BinaryTerm (Minus) (IntConstant l) (IntConstant r)) = IntConstant (l - r)
evaluate (BinaryTerm (Prod) (IntConstant 0) (IntConstant r)) = IntConstant (0) 
evaluate (BinaryTerm (Prod) (IntConstant l) (IntConstant 0)) = IntConstant (0) 
evaluate (BinaryTerm (Prod) (IntConstant l) (IntConstant r)) = IntConstant (l * r)
