import Data.Char

main :: IO()
main = print ans

ans :: Int
ans = sum . map ((\char -> char - 48) . ord) . show $ 2^1000