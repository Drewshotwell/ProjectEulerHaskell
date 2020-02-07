import Data.List

main :: IO()
main = print ans

ans :: Integer
ans = largestPalindromeProd 3

largestPalindromeProd :: Integer -> Integer
largestPalindromeProd numLen = last $ filter isPalindromeNum $ rmDuplicates [x*y | x <- range, y <- range]
   where range = [10^(numLen - 1)..(10^numLen) - 1]
         rmDuplicates = map head . group . sort

isPalindromeNum :: Integer -> Bool
isPalindromeNum num = let numStr = show num 
                      in numStr == reverse numStr