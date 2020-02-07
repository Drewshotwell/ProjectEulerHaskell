main :: IO()
main = print ans

ans :: Integer
ans = smallestDivisibleNum [1..20]

smallestDivisibleNum :: [Integer] -> Integer
smallestDivisibleNum range = cycleThruNums [1..]
   where cycleThruNums :: [Integer] -> Integer
         cycleThruNums (x : xs)
          | all (\n -> x `mod` n == 0) range = x
          | otherwise                        = cycleThruNums xs