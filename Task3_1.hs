module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
instance Show WeirdPeanoNumber where
    show Zero       = "Zero"
    show (Succ wpn) = "Succ-" ++ show wpn
    show (Pred wpn) = "Pred-" ++ show wpn

myToInteger :: WeirdPeanoNumber -> Integer
myToInteger Zero       = 0
myToInteger (Succ wpn) = myToInteger wpn + 1
myToInteger (Pred wpn) = myToInteger wpn - 1

myFromInteger :: Integer -> WeirdPeanoNumber
myFromInteger n 
    | n == 0 = Zero
    | n < 0  = Pred (myFromInteger (n + 1))
    | n > 0  = Succ (myFromInteger (n - 1))

instance Eq WeirdPeanoNumber where
    (==) wpn1 wpn2 = myToInteger wpn1 == myToInteger wpn2

instance Ord WeirdPeanoNumber where
    (<=) wpn1 wpn2 = (myToInteger wpn1) <= (myToInteger wpn2)

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize wpn = myFromInteger (myToInteger wpn)

plus :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
plus wpn1 Zero        = wpn1
plus wpn1 (Succ wpn2) = Succ (wpn1 + wpn2)
plus wpn1 (Pred wpn2) = Pred (wpn1 + wpn2)

multiply :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
multiply Zero _    = Zero
multiply _ Zero    = Zero
multiply wpn1 wpn2 = let amount = (myToInteger wpn2) in
    if amount > 0 then multiplyHelper wpn1 amount else multiplyHelper (negate wpn1) (abs amount)

multiplyHelper :: WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
multiplyHelper wpn 0 = Zero
multiplyHelper wpn n = plus wpn (multiplyHelper wpn (n - 1))

negate' :: WeirdPeanoNumber -> WeirdPeanoNumber
negate' Zero       = Zero
negate' (Succ wpn) = Pred (negate' wpn)
negate' (Pred wpn) = Succ (negate' wpn)

signum' :: WeirdPeanoNumber -> WeirdPeanoNumber
signum' Zero       = Zero
signum' (Succ wpn) = Succ Zero
signum' (Pred wpn) = Pred Zero

instance Num WeirdPeanoNumber where
    (+)         = plus
    (*)         = multiply
    abs wpn     = if wpn < Zero then negate wpn else wpn
    negate wpn  = negate' (normalize wpn)
    signum wpn  = signum' (normalize wpn)
    fromInteger = myFromInteger

instance Real WeirdPeanoNumber where
    toRational wpn = toRational (myToInteger wpn)

instance Enum WeirdPeanoNumber where
    toEnum n     = myFromInteger (fromIntegral n)
    fromEnum wpn = fromIntegral (myToInteger wpn)

divide :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
divide wpn1 wpn2 
    | wpn1 == Zero               = (Zero, Zero)
    | wpn2 == Zero               = error "error: divide by zero"
    | ((wpn1 > 0) && (wpn2 > 0)) = divideHelper wpn1 wpn2 Zero
    | ((wpn1 > 0) && (wpn2 < 0)) = negateQuot (divideHelper wpn1 (negate wpn2) Zero)
    | ((wpn1 < 0) && (wpn2 > 0)) = negateQuot (divideHelper (negate wpn1) wpn2 Zero)
    | ((wpn1 < 0) && (wpn2 < 0)) = divideHelper (negate wpn1) (negate wpn2) Zero

divideHelper :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
divideHelper wpn1 wpn2 wpn3 = if wpn1 <= wpn2 then (wpn3 , normalize (wpn2 - (wpn2 - wpn1)))
                              else divideHelper (wpn1 - wpn2) wpn2 (Succ wpn3)

negateQuot :: (WeirdPeanoNumber, WeirdPeanoNumber) -> (WeirdPeanoNumber, WeirdPeanoNumber)
negateQuot pair = (negate (fst pair), snd pair)

instance Integral WeirdPeanoNumber where
    quotRem   = divide
    toInteger = myToInteger
