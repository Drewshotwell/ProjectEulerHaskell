import Data.Maybe (fromJust)
import Data.List  (find)

main :: IO()
main = print ans

ans :: Int
ans = fromJust $ find (==maximum (collatzLengths num)) [1..num]
   where num = 999999

collatzLengths :: Int -> [Int]
collatzLengths n = map length $ map collatz [1..n - 1]

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x = x : collatz (if even x then x `div` 2
                                   else 3*x + 1)