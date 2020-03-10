import Data.Char

main :: IO ()
main = print ans

ans :: Int
ans = sum . map (\char -> ord char - 48) . show $ product[1..100]