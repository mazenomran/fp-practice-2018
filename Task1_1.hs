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
(|+|) l r = BinaryTerm Plus l r
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Minus l r
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm Prod l r
infixl 7 |*|


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
  let replace hv = replaceVar varName replacement hv in
      case expression of
        Variable variable | variable == varName -> replacement
        BinaryTerm operartion lhv rhv -> BinaryTerm operartion (replace lhv) (replace rhv)
        _ -> expression 

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
  BinaryTerm operartion lhs rhs ->
      case (operartion, left, right) of 
        (Plus, IntConstant left, IntConstant right) -> IntConstant (left + right)
        (Minus, IntConstant left, IntConstant right) -> IntConstant (left - right)
        (Prod, IntConstant left, IntConstant right) -> IntConstant (left * right)
        (Plus, IntConstant 0, right) -> right
        (Prod, IntConstant 1, right) -> right
        (Prod, IntConstant 0, right) -> IntConstant 0
        (Plus, left, IntConstant 0) -> left
        (Minus, left, IntConstant 0) -> left
        (Prod, left, IntConstant 0) -> IntConstant 0
        (Prod, left, IntConstant 1) -> left
        _ -> BinaryTerm operartion left right 
        where
          left  = evaluate lhs
          right = evaluate rhs
  _ -> expression
