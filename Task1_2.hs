module Task1_2 where

import Todo(todo)
import Data.Fixed
import Prelude hiding (sin, cos, gcd)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = calculateSin n newX s i where 
    newX = mod' x (2 * pi) :: Double
    n = newX :: Double
    s = 0.0 :: Double
    i = 1 :: Int

calculateSin :: Double -> Double -> Double -> Int -> Double
calculateSin n x s i = if (abs n < eps) then s 
    else calculateSin (n * (generalNum x / denomForSin i)) x (s + n) (i + 1)
    where eps = 1e-8 :: Double

generalNum :: Double -> Double
generalNum x = (-1.0) * x * x

denomForSin :: Int -> Double
denomForSin i = ((2 * (fromIntegral i)) * (2 * (fromIntegral i) + 1))

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = calculateCos n newX s i where
    newX = mod' x (2 * pi) :: Double
    n = 1.0 :: Double
    s = 0.0 :: Double
    i = 1 :: Int

calculateCos :: Double -> Double -> Double -> Int -> Double
calculateCos n x s i = if abs n < eps then s 
    else calculateCos (n * (generalNum x / denomForCos i)) x (s + n) (i + 1) 
    where eps = 1e-8 :: Double

denomForCos :: Int -> Double
denomForCos i = ((2 * (fromIntegral i) - 1) * (2 * (fromIntegral i)))

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y 
    | x == 0    = y
    | y == 0    = x
    | otherwise = gcd y (mod x y)
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
isDateCorrect day month year = 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y 
    | y == 0 = 1
    | y == 1 = x
    | even y = pow (x * x) (y `div` 2)
    | odd y  = x * pow (x * x) ((y - 1) `div` 2)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = todo

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
