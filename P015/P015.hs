main :: IO ()
main = print ans

ans :: Integer
ans = choose (2*size) size
   where size = 20

choose :: Integer -> Integer -> Integer
choose n k = fact n `div` (fact k * fact (n - k))

fact :: Integer -> Integer
fact n = product [1..n]