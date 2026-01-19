import Task10
import Task20
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Task 10 (Sum of Primes)" $ do
    it "isPrime works correctly" $ do
      isPrime 1 `shouldBe` False
      isPrime 2 `shouldBe` True
      isPrime 3 `shouldBe` True
      isPrime 4 `shouldBe` False
      isPrime 17 `shouldBe` True

    it "Calculates sum of primes below 10 (Example: 17)" $ do
      sumPrimesModular 10 `shouldBe` 17
      sumPrimesTailRec 10 `shouldBe` 17
      sumPrimesListComp 10 `shouldBe` 17
      sumPrimesLazy 10 `shouldBe` 17

    it "Calculates sum of primes below 20000 (Sanity Check)" $ do
      -- 2262 + сумма других чисел. Просто проверяем совпадение методов
      let res = sumPrimesModular 20000
      sumPrimesTailRec 20000 `shouldBe` res
      sumPrimesLazy 20000 `shouldBe` res

  describe "Task 20 (Factorial Digit Sum)" $ do
    it "Calculates digit sum of 10! (Example: 3628800 -> 27)" $ do
      solve20Modular 10 `shouldBe` 27
      solve20TailRec 10 `shouldBe` 27
      solve20ListComp 10 `shouldBe` 27
      solve20Lazy 10 `shouldBe` 27

    it "Calculates digit sum of 100! (Expected: 648)" $ do
      solve20Modular 100 `shouldBe` 648
      solve20Lazy 100 `shouldBe` 648
