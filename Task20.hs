module Main where

import Data.List (foldl')
import Data.Char (digitToInt)
import System.IO (hFlush, stdout)
import GHC.IO.Encoding (setLocaleEncoding, utf8) 

-- ЗАДАЧА 2: Сумма цифр факториала N!

sumDigits :: Integer -> Int
sumDigits n = sum (map digitToInt (show n))


-- 1a. Монолитная реализация: Хвостовая рекурсия
factTailRec :: Integer -> Integer -> Integer
factTailRec 0 acc = acc
factTailRec k acc = factTailRec (k - 1) (acc * k)

solve20TailRec :: Integer -> Int
solve20TailRec n = sumDigits (factTailRec n 1)

-- 1b. Монолитная реализация: Обычная рекурсия
factRec :: Integer -> Integer
factRec 0 = 1
factRec k = k * factRec (k - 1)

solve20Rec :: Integer -> Int
solve20Rec n = sumDigits (factRec n)

-- 2. Модульная реализация (Fold)
-- Свертка списка чисел с умножением для получения факториала.
solve20Modular :: Integer -> Int
solve20Modular n = 
    let factorial = foldl' (*) 1 [1 .. n]
    in sumDigits factorial

-- 3. Генерация через Map
solve20Map :: Integer -> Int
solve20Map n = sum $ map digitToInt $ show $ product [1..n]

-- 4. Спец. синтаксис: List Comprehension
solve20ListComp :: Integer -> Int
solve20ListComp n = sum [digitToInt c | c <- show (product [1..n])]

-- 5. Бесконечные списки
solve20Lazy :: Integer -> Int
solve20Lazy n = sumDigits (factorialsInfinite !! (fromIntegral n - 1))
  where
    factorialsInfinite = scanl1 (*) [1..]

-- MAIN

main :: IO ()
main = do
    setLocaleEncoding utf8

    putStr "Введите N (например, 100): "
    hFlush stdout
    input2 <- getLine
    let n = read input2 :: Integer

    putStrLn "\n--- Результаты вычислений ---"
    putStrLn $ "1. Хвостовая рекурсия: " ++ show (solve20TailRec n)
    putStrLn $ "2. Рекурсия:           " ++ show (solve20Rec n)
    putStrLn $ "3. Модульная (Fold):   " ++ show (solve20Modular n)
    putStrLn $ "4. Map:                " ++ show (solve20Map n)
    putStrLn $ "5. List Comprehension: " ++ show (solve20ListComp n)
    putStrLn $ "6. Lazy (Infinite):    " ++ show (solve20Lazy n)