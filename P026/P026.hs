import Data.List
import Data.Numbers.Primes

main :: IO ()
main = print ans

ans :: [(Integer, Integer)]
ans = longestRepFraction [7..999]

longestRepFraction :: [Integer] -> [(Integer, Integer)]
longestRepFraction xs = maximumBy (\t1 t2 -> compare (fst t1) (fst t2)) $
                        zip (map (\x -> divNineReps x 1) primeXs) primeXs
   where primeXs = filter isPrime xs

divNineReps :: Integer -> Integer -> Integer
divNineReps x n = if (nineReps n `mod` x) == 0 
                  then n
                  else divNineReps x (n + 1)
   where nineReps :: Integer -> Integer
         nineReps reps = read . take (fromInteger reps) $ repeat '9'