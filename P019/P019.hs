{-
   * 1 Jan 1900 was a Monday.
   * Thirty days has September,
   * April, June and November.
   * All the rest have thirty-one,
   * Saving February alone,
   * Which has twenty-eight, rain or shine.
   * And on leap years, twenty-nine.
   * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-}

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
   deriving (Eq, Enum)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
   deriving (Eq, Enum)

main :: IO ()
main = print ans

ans :: Int
ans = sundaysFirstOfMonth [1900..2000]

sundaysFirstOfMonth :: [Int] -> Int
sundaysFirstOfMonth elaspedYears = length . filter (==(Sunday, 1)) . drop 365 $
                                   yearWeekCycle elaspedYears Monday

yearWeekCycle :: [Int] -> DayOfWeek -> [(DayOfWeek, Int)]
yearWeekCycle []       _        = []
yearWeekCycle (yr:yrs) firstDay = yearWeekSet ++
                                  yearWeekCycle yrs (succWeekCycle . fst . last $ yearWeekSet)
   where weekCycle = concat . repeat $ 
                              [firstDay..] ++ 
                              takeWhile (/= firstDay) [(Monday::DayOfWeek)..]
         yearWeekSet = take (sum . map (\month -> getDaysInMonth month yr) $ [(January::Month)..(December::Month)]) $
                            zip
                              (weekCycle)
                              (concatMap (\month -> [1..getDaysInMonth month yr])
                                 . concat . repeat $ [(January::Month)..(December::Month)])

succWeekCycle :: DayOfWeek -> DayOfWeek
succWeekCycle Sunday = Monday
succWeekCycle day = succ day

getDaysInMonth :: Month -> Int -> Int
getDaysInMonth February year
   | (year `mod` 100 /= 0 && year `mod` 4 == 0) ||
     (year `mod` 400 == 0)
               = 29
   | otherwise = 28
getDaysInMonth month _
   | month == September || 
     month == April || 
     month == June || 
     month == November
               = 30
   | otherwise = 31