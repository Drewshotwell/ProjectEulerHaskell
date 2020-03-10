main :: IO ()
main = print ans

ans :: Int
ans = (read . reverse . take 10 . reverse . 
      show . sum $ zipWith (^) [1..1000] [1..1000]) :: Int