main :: IO ()
main = print ans

ans :: [(Int, Int)]
ans = amicablePairs [1..10000] [1..10000]

amicablePairs :: [Int] -> [Int] -> [(Int, Int)]
amicablePairs _ []           = []
amicablePairs (l:ls) (sl:sls) = []

properFactors :: Int -> [Int]
properFactors n = [x | x <- if even n then [1..n `div` 2]
                            else           [1..n `div` 3]
                            , n `mod` x == 0]
