import Text.Regex

main :: IO ()
main = do
    namesStr <- readFile "/home/drewshotwell/Dropbox/ProjectEulerHaskell/P022/p022_names.txt"
    namesList <- splitRegex (mkRegex ",") namesStr
    print $ namesList !! 0

ans :: String -> Int
ans str = 0