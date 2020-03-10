import Data.Numbers.Primes
import Data.List

main :: IO ()
main = print ans

ans :: Int
ans =
   let rangeA = [-999..999]
       rangeB = [-1000..1000]
   in  coefProduct rangeA rangeB

coefProduct :: [Int] -> [Int] -> Int
coefProduct as bs = (\t -> (fst t)*(snd t)) $
                    maximumBy comparatorByPrimes [(a,b) | a <- as, b <- bs, isPrime . abs $ a, isPrime . abs $ b]

comparatorByPrimes :: (Int, Int) -> (Int, Int) -> Ordering
comparatorByPrimes t1 t2 =
   let stretchOfPrimes = \t -> takeWhile isPrime $
                         map (quadracticWith (fst t) (snd t)) [0..]
   in  compare (length . stretchOfPrimes $ t1) (length . stretchOfPrimes $ t2)

quadracticWith :: Int -> Int -> (Int -> Int)
quadracticWith a b = \n -> n*n + a*n + b