fractionalList = [1.0, 3.5, 2.4, 8.9, 14.5, 15.2, 13.2]
computeMean :: Fractional a => [a] -> a
computeMean list = sum list / fromIntegral (length list) --length list gives back and integer but we need the same type
                                                         --as sum list, so we turn the type to the expected one with fromIntegral

removeOccurence :: Eq a => a -> [a] -> [a] -- List of elements that support equality operators (Eq)
removeOccurence x list = filter (/= x) list --filter needs x in Eq type class, filters all x from list

replaceChars :: String -> String
replaceChars word = "<" ++ smallerWord ++ ">"
    where smallerWord = init (tail word) --init drops the last char of the word while tail drops the first

generatePrime :: Int -> [Int] 
generatePrime n = filterMult [2..n]
    where 
        filterMult [] = [] --handle empty case
        filterMult (x:xs) = x : filterMult [y | y <- xs, mod y x /= 0] 
        -- removes multiples of head of list, then recursively calls itself with next
        -- element in list as head and removes its multiples until list is empty

checkCoprime :: Int -> Int -> Bool
checkCoprime x y 
    | gcd x y == 1 = True -- if gcd is 1 then the two numbers are coprime
    | otherwise = False

multOf6and10 :: Int -> [Int]
multOf6and10 n = [x | x <- [1..n], mod x 6 == 0 || mod x 10 == 0]

main :: IO()
main = do 
    print (computeMean fractionalList)
    print (removeOccurence 5 [5, 2, 8, 4, 5])
    print (replaceChars "Hihaho")
    print (generatePrime 20)
    print (checkCoprime 4 8)
    print (multOf6and10 999)
