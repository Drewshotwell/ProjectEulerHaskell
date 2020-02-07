main :: IO ()
main = print ans

ans :: Int
ans = sumOfWordNums [1..1000]

sumOfWordNums :: [Int] -> Int
sumOfWordNums nums = sum $ map (length . digitListToWord . show) nums

digitListToWord :: String -> String
digitListToWord "1000" = "onethousand"
digitListToWord [n0] = ones n0
digitListToWord [n1, n0]
   | n1 == '1' && n0 > '0'  = teens n0
   | otherwise              = tens n1 ++ ones n0
digitListToWord [n2, n1, n0]
   | n1 > '0' || n0 > '0'   = ones n2 ++ "hundredand" ++ digitListToWord [n1, n0]
   | otherwise              = ones n2 ++ "hundred"

ones :: Char -> String
ones n = case n of  '0' -> ""
                    '1' -> "one"
                    '2' -> "two"
                    '3' -> "three"
                    '4' -> "four"
                    '5' -> "five"
                    '6' -> "six"
                    '7' -> "seven"
                    '8' -> "eight"
                    '9' -> "nine"

teens :: Char -> String
teens n = case n of '1' -> "eleven"
                    '2' -> "twelve"
                    '3' -> "thirteen"
                    '4' -> "fourteen"
                    '5' -> "fifteen"
                    '6' -> "sixteen"
                    '7' -> "seventeen"
                    '8' -> "eighteen"
                    '9' -> "nineteen"

tens :: Char -> String
tens n = case n of  '0' -> ""
                    '1' -> "ten"
                    '2' -> "twenty"
                    '3' -> "thirty"
                    '4' -> "forty"
                    '5' -> "fifty"
                    '6' -> "sixty"
                    '7' -> "seventy"
                    '8' -> "eighty"
                    '9' -> "ninety"