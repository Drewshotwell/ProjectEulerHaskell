module Main where

main :: IO()
main = print ans

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib(x-2)

evenFibs = [fib n | n <- [1..], even $ fib n]

ans = sum $ takeWhile (<= upperBound) evenFibs
    where upperBound = 4000000