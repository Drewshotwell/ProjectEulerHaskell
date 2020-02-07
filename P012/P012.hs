import Data.Maybe
import Data.List
import Data.Numbers.Primes

main :: IO()
main = print ans

ans :: Int
ans = fromJust $ find ((>500) . factorsLen) triangularNumbers

factorsLen :: Int -> Int
factorsLen = product . map ((+1) . length) . group . primeFactors

triangularNumbers :: [Int]
triangularNumbers = [(x*(x+1)) `div` 2 | x <- [1..]]