import Data.List
import Data.Char
import Data.Function

main :: IO ()
main = print ans

--ans :: Int
ans = bCom 3

bCom :: Int -> [[Int]]
bCom 0 = [[]]
bCom n = [0,1] >>= \b -> map (b:) (bCom (n - 1))

bitShift :: [Int] -> [Int]
bitShift [] = []
bitShift [b] = b:[0]
bitShift (_:bs) = bs ++ [0]

shiftEquivalent :: [Int] -> [Int] -> Bool
shiftEquivalent bs1 bs2 = drop 1 bs1 == take (length bs2 - 1) bs2

numToDigits :: Int -> [Int]
numToDigits num = map digitToInt (show num)

connectNodes :: [[Int]] -> [[[Int]]]
connectNodes [] = [[]]
connectNodes (bSeq:bSeqs) = 
    let bSeqsThatFit = filter (shiftEquivalent bSeq) bSeqs
    in  bSeqsThatFit >>= \b -> map (b :) (connectNodes bSeqs)--map (bSeq:) bSeqsThatFit

digitsToNum :: [Int] -> Int
digitsToNum numL = (read . concatMap show $ numL) :: Int