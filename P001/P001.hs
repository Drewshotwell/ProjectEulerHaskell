module Main where

main :: IO()
main = print ans

dividesBy :: Int -> Int -> Bool
dividesBy n m = (n `mod` m == 0)

ans = sum [n | n <- [1..999], n `dividesBy` 3 || n `dividesBy` 5]