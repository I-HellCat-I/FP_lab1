module Task20
  ( solve20Lazy,
    solve20ListComp,
    solve20Map,
    solve20Modular,
    solve20Rec,
    solve20TailRec,
    sumDigits,
  )
where

import Data.Char (digitToInt)

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
solve20Map n = sum $ map digitToInt $ show $ product [1 .. n]

-- 4. Спец. синтаксис: List Comprehension
solve20ListComp :: Integer -> Int
solve20ListComp n = sum [digitToInt c | c <- show (product [1 .. n])]

-- 5. Бесконечные списки
solve20Lazy :: Integer -> Int
solve20Lazy n = sumDigits (factorialsInfinite !! (fromIntegral n - 1))
  where
    factorialsInfinite = scanl1 (*) [1 ..]
