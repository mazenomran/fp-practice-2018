module Task1_2 where

import Todo(todo)
import Data.Fixed

-- синус числа (формула Тейлора)
mySin :: Double -> Int -> Double
mySin x 0 = 1
mySin x n = mySin x (n-1) + (-1)^n * x^(2*n+1) / (fromIntegral $ product [1..(2*n+1)])

-- косинус числа (формула Тейлора)
myCos :: Double -> Int -> Double
myCos x 0 = 1
myCos x n = myCos x (n-1) + (-1)^n * x^(2*n) / (fromIntegral $ product [1..2*n])

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y = if (x == 0) then y
          else if (y == 0) then x
               else myGCD y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to 
    | from == to    = False
    | mod' x 1 == 0 = True 
    | otherwise     = doesSquareBetweenExist (from + 1) to
    where x = sqrt (fromIntegral from)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y 
                   |(elem (d [1..30]) && m<13 && elem (m [4,6,3,11]) && y>0) = True
                   |(m ==2 && elem (d [1..28]) && y>0 && mod y 4 /= 0) = True
                   |(m ==2 && elem (d [1..29]) && y>0 && mod y 4 == 0) = True
                   |(elem (d [1..31]) && elem (m [1,3,5,7,8,10,12]) && y>0) = True
                    otherwise let isDateCorrect d m y = False
                       
-- является ли данное число простым?

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | (length [n | n <- [2 .. x-1], mod x n == 0]) > 0 = False
          | otherwise = True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя

pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x y | even y = pow (x*x) (y `div` 2)
        | otherwise = x * (pow (x*x) ((y-1) `div` 2))





type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
