module Main where

import Data.List (foldl')
import Data.Char (digitToInt)
import System.IO (hFlush, stdout)
import GHC.IO.Encoding (setLocaleEncoding, utf8) 


-- ЗАДАЧА 1: Сумма простых чисел меньше N

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = all (\x -> n `mod` x /= 0) [3, 5 .. floor (sqrt (fromIntegral n))]

-- 1a. Монолитная реализация: Хвостовая рекурсия
sumPrimesTailRec :: Int -> Int
sumPrimesTailRec limit = go 2 0
  where
    go current acc
        | current >= limit = acc
        | isPrime current  = go (current + 1) (acc + current)
        | otherwise        = go (current + 1) acc

-- 1b. Монолитная реализация: Обычная рекурсия
sumPrimesRec :: Int -> Int -> Int
sumPrimesRec limit current
    | current >= limit = 0
    | isPrime current  = current + sumPrimesRec limit (current + 1)
    | otherwise        = sumPrimesRec limit (current + 1)

-- 2. Модульная реализация (Filter + Fold)
sumPrimesModular :: Int -> Int
sumPrimesModular limit = foldl' (+) 0 (filter isPrime [2 .. (limit - 1)])

-- 3. Генерация через Map
sumPrimesMap :: Int -> Int
sumPrimesMap limit = sum (map (\x -> if isPrime x then x else 0) [2 .. (limit - 1)])

-- 4. Спец. синтаксис: List Comprehension
sumPrimesListComp :: Int -> Int
sumPrimesListComp limit = sum [x | x <- [2 .. (limit - 1)], isPrime x]

-- 5. Бесконечные списки (Lazy Evaluation)
sumPrimesLazy :: Int -> Int
sumPrimesLazy limit = sum (takeWhile (< limit) primesInfinite)
  where
    primesInfinite = filter isPrime [2..]


main :: IO ()
main = do
    setLocaleEncoding utf8
    putStr "Введите Limit (например, 2000000): "
    hFlush stdout
    input1 <- getLine
    let limit = read input1 :: Int

    putStrLn "\n--- Результаты вычислений ---"
    putStrLn $ "1. Хвостовая рекурсия: " ++ show (sumPrimesTailRec limit)
    -- Закомментил, ибо нюка
    putStrLn $ "2. Рекурсия (Stack):   " ++ show (sumPrimesRec limit 0) 
    putStrLn $ "3. Модульная (Fold):   " ++ show (sumPrimesModular limit)
    putStrLn $ "4. Map:                " ++ show (sumPrimesMap limit)
    putStrLn $ "5. List Comprehension: " ++ show (sumPrimesListComp limit)
    putStrLn $ "6. Lazy (Infinite):    " ++ show (sumPrimesLazy limit)
