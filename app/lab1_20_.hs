import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Char (ord)

main :: IO ()
main = do
    rawContent <- readFile "names.txt"
    let result =
            sum $
            zipWith ( \position name -> position * alphabeticalValue name) [1..]
            (sort $ splitOn "," (filter (/= '"') rawContent))
    print result


alphabeticalValue :: String -> Int
alphabeticalValue name = sum $ map charValue name
  where
    charValue char = ord char - ord 'A' + 1