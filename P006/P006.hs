main :: IO()
main = print ans

ans :: Integer
ans = sumOfSquareDiff [1..100]

sumOfSquareDiff :: [Integer] -> Integer
sumOfSquareDiff range = (sum $ map (^2) range) - (sum range)^2