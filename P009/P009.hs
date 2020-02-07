main :: IO()
main = print ans

ans :: Int
ans = sumOfPyTriplesEqual 1000

sumOfPyTriplesEqual :: Int -> Int
sumOfPyTriplesEqual val = prodOfTriple . head $ filter ((== val) . sumOfTriple) [(m^2 - n^2, 2*m*n, m^2 + n^2) | m <- [2..1000], n <- [1..1000], m > n]
      where sumOfTriple (a, b, c) = a + b + c
            prodOfTriple (a, b, c) = a * b * c