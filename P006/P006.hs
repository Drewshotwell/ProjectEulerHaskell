main :: IO()
main = print ans

ans :: Integer
ans = sumOfSquareDiff [1..100]

sumOfSquareDiff :: [Integer] -> Integer
sumOfSquareDiff range = sumSquared - sumOfSquares
   where sumOfSquares :: Integer
         sumOfSquares = sum $ map (^2) range
         sumSquared :: Integer
         sumSquared = (sum range)^2