import Data.Char
import Data.Maybe
import Data.List
import Data.Function

main :: IO ()
main = print ans

-- Problem description

ans :: Integer
ans = g' 5

f :: Integer -> Integer
f = sum . map (\k -> facMemo !! (fromIntegral k)) . numToDigits

sf :: Integer -> Integer
sf = s . f

g' :: Integer -> Integer -- Naive solution
g' i = fromJust . find (\n -> sf n == i) $ [1..]

-- for cur n, if (not $ 0 `elem` numToDigits n) && isSorted n then equals n if sf n = 1, otherwise g'' n + 1
g'' :: Integer -> Integer -> Integer
g'' n0 i = 
    if (not $ 0 `elem` numToDigits n0) && isSorted n0
        then 
            if sf n0 == i then n0
            else g'' (n0 + 1) i
        else g'' (n0 + 1) i

combs = sort . concatMap (\(x,y) -> [y * k | k <- [1..x]]) $ tail facMemoPair

gf' :: Integer -> Integer
gf' i = fromJust . find (\n -> f n == i) $ [1..]

sg :: Integer -> Integer
sg = s . (g'' 1)

sgTo :: Integer -> Integer
sgTo k = sum . map sg $ [1..k]

-- Utility --

fac :: Integer -> Integer
fac n = product [1..n]

facMemo :: [Integer]
facMemo = map fac [0..9]

facMemoPair :: [(Integer, Integer)]
facMemoPair = zip [0..9] facMemo

s :: Integer -> Integer
s = toInteger . sum . numToDigits

numToDigits :: Integer -> [Int]
numToDigits num = map digitToInt (show num)

digitsToNum :: [Integer] -> Integer
digitsToNum numL = (read . concatMap show $ numL) :: Integer

sortedEquivalent :: Integer -> Integer -> Bool
sortedEquivalent = (==) `on` (sort . numToDigits)

-- N without sorted equivalents or 0
filteredNaturals :: [Integer] -> [Integer]
--filteredNaturals = foldl (\acc x -> acc) []
filteredNaturals = 
    foldl (\acc x ->
        if  (0 `elem` numToDigits x) || 
            any (\y -> sortedEquivalent x y) acc
        then acc 
        else acc ++ [x])
    []

-- Heavy lifting functions --
isSorted :: Integer -> Bool
--isSorted n = (numToDigits n) == (sort . numToDigits $ n)
isSorted n = let numL = (numToDigits n) in all (\i -> (numL !! i) <= (numL !! (i + 1))) [0..length numL - 2]

--g :: Integer -> Integer -- Rigourous solution
g = map (digitsToNum . snd . (\n -> subtractFacts (n, [])) . digitsToNum) . partitions

subtractFacts :: (Integer, [Integer]) -> (Integer, [Integer]) -- f inverse
subtractFacts (0, lst)        = (0, lst)
subtractFacts (startNum, lst) =
    let leastFac = last $ facMemoPair \\ filter (\n -> snd n > startNum) facMemoPair
    in  subtractFacts (startNum - snd leastFac, fst leastFac : lst)

partitionsFrom :: Integer -> Integer -> [[Integer]]
partitionsFrom _ n | n == 0 = [[]]
partitionsFrom i n | n < i = []
partitionsFrom i n =
  map (i :) (partitionsFrom i (n - i)) ++ partitionsFrom (i + 1) n

partitions :: Integer -> [[Integer]]
partitions i = nub . concatMap permutations . filter hasLTNRepetitions . filter (not . any (>9)) $ partitionsFrom 1 i

hasLTNRepetitions :: [Integer] -> Bool
hasLTNRepetitions = foldl (&&) True . map (\ns -> length ns <= head ns) . group . map fromIntegral

-- The esoteric functions --

weirdGroupingF :: [Integer] -> [[(Integer, Integer, Integer)]]
weirdGroupingF range =
    --map (head . groupBy (\(n1, _, _) (n2, _, _) -> sortedEquivalent n1 n2)) $ 
    groupBy (\(_, fV1, _) (_, fV2, _) -> fV1 == fV2) $ 
    sortOn (\(_, fV, _) -> fV) $ 
    zip3 range (map f range) (map sf range)

weirdGroupingSF :: [Integer] -> [[(Integer, Integer, Integer)]]
weirdGroupingSF range =
    --map (head . groupBy (\(n1, _, _) (n2, _, _) -> sortedEquivalent n1 n2)) $ 
    groupBy (\(_, _, sfV1) (_, _, sfV2) -> sfV1 == sfV2) $ 
    sortOn (\(_, _, sfV) -> sfV) $ 
    zip3 range (map f range) (map sf range)

--kCoefs :: Integer -> [[(Integer, Integer)]]
--kCoefs = map (map (\k -> (length k, head k)) . group) . partitions

--kCoefss :: [(Integer, Integer)] -> Integer
--kCoefss = foldl (\acc x -> acc + ((s) $ (facMemo !! (snd x))*fst x)) 0

sumCombinations :: Integer -> [[Integer]]
sumCombinations 1 = [[1]]
sumCombinations i = filter (not . any (>9)) $ initialList : (nub . map sort . collapseCombinations $ initialList)
    where initialList = take (fromIntegral i) $ repeat 1

partitionsForN :: Integer -> [Integer]
partitionsForN n = 
    let facMemoCombinations = concat [scanr (+) k . take (fromJust $ elemIndex k facMemo) $ facMemo | k <- facMemo]
        facDistList = filter (\k -> s (fst k) <= n && s (fst k + snd k) == n) [(k, distanceToN k n) | k <- facMemoCombinations]
    in  map (\k -> fst k + snd k) facDistList

distanceToN :: Integer -> Integer -> Integer
distanceToN k x = abs (s k - x)

collapseCombinations :: [Integer] -> [[Integer]]
collapseCombinations xs
    | (length $ tail computed) == 0 = [computed]
    | (length $ tail computed) > 1  = computed : collapseCombinations computed ++ map ((head computed):) (collapseCombinations (tail computed))
    | otherwise                     = computed : collapseCombinations computed
    where computed = (\(y:ys) -> (y + head ys) : (ys \\ [head ys])) xs

filterOutSortedEquivs :: [Integer] -> [Integer]
filterOutSortedEquivs [] = []
filterOutSortedEquivs (n:ns) =
    let sortedEquivs = filter (sortedEquivalent n) ns
    in  n : (ns \\ sortedEquivs)