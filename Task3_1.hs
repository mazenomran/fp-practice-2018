module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
instance Show WeirdPeanoNumber where
    show Zero       = "Zero"
    show (Succ x) = "Succ-" ++ show x
    show (Pred x) = "Pred-" ++ show x

toInt :: WeirdPeanoNumber -> Integer
toInt Zero = 0
toInt (Succ x) = (toInt x) + 1
toInt (Pred x) = (toInt x) - 1

fromInt :: Integer -> WeirdPeanoNumber
fromInt x | x > 0 = Succ $ fromInt (x - 1)
          | x < 0 = Pred $ fromInt (x + 1)
          | otherwise = Zero

reduce :: WeirdPeanoNumber -> WeirdPeanoNumber
reduce Zero = Zero
reduce (Succ (Pred x)) = reduce x
reduce (Pred (Succ x)) = reduce x
reduce (Succ a) = case reduce a of (Pred b) -> b
                                   _ -> Succ $ reduce a
reduce (Pred a) = case reduce a of (Succ b) -> b
                                   _ -> Pred $ reduce a


instance Eq WeirdPeanoNumber where
    (==) a b = reduce a `equal` reduce b where
      equal Zero Zero = True
      equal Zero _ = False
      equal _ Zero = False
      equal (Succ a) (Succ b) = equal a b
      equal (Pred a) (Pred b) = equal a b
      equal _ _ = False

instance Ord WeirdPeanoNumber where
    (<=) a b = reduce a `lessorequal` reduce b where
      lessorequal (Succ a) (Succ b) = lessorequal a b
      lessorequal (Pred a) (Pred b) = lessorequal a b
      lessorequal (Pred _) Zero = True
      lessorequal Zero (Succ _) = True
      lessorequal a b = a == b

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral -- Возьмёт из Num
    fromEnum = fromInteger.toInt

instance Num WeirdPeanoNumber where
    Zero + a = a
    a + Zero = a
    (Pred a) + b = a + (Pred b)
    (Succ a) + b = a + (Succ b)

    signum a = case reduce a of (Succ _) -> Succ Zero
                                (Pred _) -> Pred Zero
                                _        -> Zero

    negate Zero = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a

    abs a = if signum a < Zero then negate a else a

    a * b = case signum b of (Succ Zero) -> a + a * (Pred b)
                             (Pred Zero) -> negate $ a * (abs b)
                             (Zero)      -> Zero

    fromInteger = fromInt

instance Real WeirdPeanoNumber where
    toRational = toRational.toInt

instance Integral WeirdPeanoNumber where
    toInteger = toInt


    quotRem a b | signum a == signum b = result
                | otherwise = (negate $ fst result, snd result)
                   where absQuotRem res@(quot, rem) b | rem >= b = absQuotRem (quot + 1, rem - b) b
                                                      | otherwise = res
                         result = absQuotRem (Zero, abs a) (abs b)
