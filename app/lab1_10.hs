main :: IO ()
main = print (sumOfPrimesUpTo 2000000)

sumOfPrimesUpTo :: Integer -> Integer
sumOfPrimesUpTo n = 2 + sum (filter isPrime [3,5..n])


isPrime :: Integer -> Bool
isPrime n | n < 2     = False
          | n == 2    = True
          | even n    = False
          | otherwise = go 3
  where
    go d | d * d > n = True
         | n `mod` d == 0 = False
         | otherwise      = go (d + 2)