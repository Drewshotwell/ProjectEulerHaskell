main :: IO()
main = print ans

ans :: Int
ans = prime 10001

prime :: Int -> Int
prime n = last . take n $ sieve [2..]
   where sieve [] = []
         sieve (p : ns) = p : sieve [x | x <- ns, x `mod` p /= 0]