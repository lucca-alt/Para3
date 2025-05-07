convert :: Int -> String
convert totalSeconds
    | totalSeconds < 0 = "Invalid input"
    | otherwise = showTime (hours, minutes, seconds) ++ " " ++ whatDay
        where 
            weekdays = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
            whatDay = head [day | (dayIndex, day) <- zip [0..] weekdays, dayIndex == (totalSeconds `div` 86400) `mod` 7]
            -- computes the index to the matching day and takes it out of the list comprehension with head

            hours = (totalSeconds `div` 3600) `mod` 24 -- mod 24 because else it would go past 24
            minutes = (totalSeconds `mod` 3600) `div` 60
            seconds = (totalSeconds `mod` 3600) `mod` 60

showTime :: (Int, Int, Int) -> String
showTime (hours, minutes, seconds) = add0 hours ++ ":" ++ add0 minutes ++ ":" ++ add0 seconds 
    where 
        add0 x = if x < 10 then "0" ++ show x else show x


main :: IO()
main = do
    putStrLn "Enter an amount of seconds to show the time and day: "
    input <- getLine
    let seconds = read input :: Int
    putStrLn ("Your time and day are: " ++ show (convert seconds))
    putStrLn "Now how many seconds do you want to go back in time?: "
    input2 <- getLine
    let updatedSeconds = read input2 :: Int
    putStrLn ("Your updated time is: " ++ show (convert (seconds - updatedSeconds)))
