main :: IO ()
main = print ans

ans :: Int
ans = 0

trioIsPanDigital :: Int -> Int -> Int -> Bool
trioIsPanDigital x y z =
   let parsedDigitString = ((show x) ++ (show y) ++ (show z)) :: String
   in  noDuplicates parsedDigitString []

noDuplicates :: String -> String -> Bool
noDuplicates [] _ = True
noDuplicates (c:cs) collectedChrs =
   if all (/= c) collectedChrs then noDuplicates cs (c:collectedChrs)
   else False