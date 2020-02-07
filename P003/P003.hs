main :: IO()
main = print ans

ans :: Integer
ans = largestPrimeFactorOf 600851475143

largestPrimeFactorOf :: Integer -> Integer
largestPrimeFactorOf num = filterLastPrimeFactor $ reverse [3, 5..floorRoot num]
   where filterLastPrimeFactor :: [Integer] -> Integer
         filterLastPrimeFactor [] = num
         filterLastPrimeFactor (n : ns)
          | num `mod` n == 0 && isPrime n = n
          | otherwise = filterLastPrimeFactor ns

isPrime :: Integer -> Bool
isPrime num = null (checkDivisibilityThruList [2..floorRoot num])
   where checkDivisibilityThruList :: [Integer] -> [Integer]
         checkDivisibilityThruList [] = []
         checkDivisibilityThruList (n : ns)
          | num `mod` n == 0  = n : (checkDivisibilityThruList ns)
          | otherwise         = checkDivisibilityThruList ns

floorRoot :: Integer -> Integer
floorRoot = floor . sqrt . fromIntegral