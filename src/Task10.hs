module Task10
  ( isPrime,
    sumPrimesLazy,
    sumPrimesListComp,
    sumPrimesMap,
    sumPrimesModular,
    sumPrimesRec,
    sumPrimesTailRec,
  )
where

-- ЗАДАЧА 1: Сумма простых чисел меньше N

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\x -> n `mod` x /= 0) [3, 5 .. floor (sqrt (fromIntegral n :: Double))]

-- 1a. Монолитная реализация: Хвостовая рекурсия
sumPrimesTailRec :: Int -> Int
sumPrimesTailRec limit = go 2 0
  where
    go current acc
      | current >= limit = acc
      | isPrime current = go (current + 1) (acc + current)
      | otherwise = go (current + 1) acc

-- 1b. Монолитная реализация: Обычная рекурсия
sumPrimesRec :: Int -> Int -> Int
sumPrimesRec limit current
  | current >= limit = 0
  | isPrime current = current + sumPrimesRec limit (current + 1)
  | otherwise = sumPrimesRec limit (current + 1)

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
    primesInfinite = filter isPrime [2 ..]
