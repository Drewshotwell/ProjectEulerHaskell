import Text.Regex
import Data.List
import Data.Char

main :: IO ()
main = do
   namesStr <- readFile $ "./P022/p022_names.txt"
   print
      . rankNames . map (filter (/= '"')) 
         $ splitRegex (mkRegex ",") namesStr

rankNames :: [String] -> Int
rankNames names = sum $ zipWith (*) 
                        [sum . map (\char -> ord char - 64) $ sortedName
                           | sortedName <- sort names]
                        [1..length names]