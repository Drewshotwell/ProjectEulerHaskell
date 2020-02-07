main :: IO()
main = print ans

ans :: Integer
ans = sum $ filter isPrime (2 : [3, 5..2000000])

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime num = null (checkDivisibilityThruList [2..floorRoot num])
   where checkDivisibilityThruList :: [Integer] -> [Integer]
         checkDivisibilityThruList [] = []
         checkDivisibilityThruList (n : ns)
          | num `mod` n == 0 = n : (checkDivisibilityThruList ns)
          | otherwise        = checkDivisibilityThruList ns

floorRoot :: Integer -> Integer
floorRoot = floor . sqrt . fromIntegral