import Data.Numbers.Primes
import Data.List

main :: IO ()
main = print ans

ans :: Int
ans = length $ setOfCircularPrimesUnder 1000000

setOfCircularPrimesUnder :: Int -> [Int]
setOfCircularPrimesUnder n =
   let primesUnderN = takeWhile (<n) primes
   in  processList primesUnderN
      where processList []     = []
            processList (x:xs) = if isCircularPrime x
                                 then (nPerms x) ++ processList (xs \\ (nPerms x))
                                 else processList xs

isCircularPrime :: Int -> Bool
isCircularPrime n = all isPrime . tail $ nPerms n

nPerms :: Int -> [Int]
nPerms n = rmdups . map (\s -> read s :: Int) . permutations $ show n

rmdups :: [Int] -> [Int]
rmdups []     = []
rmdups (a:as) = a : rmdups (as \\ take (length . filter (==a) $ as) (repeat a))