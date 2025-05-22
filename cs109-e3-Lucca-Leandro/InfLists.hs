checkPrime :: Int -> Bool
checkPrime n 
    | n < 2 = False
    | otherwise = [x | x <- [1..n], mod n x == 0] == [1,n] 
    -- ==[1,n] checks if only divisors are 1 and n itself, if true then its prime

genInfPrime :: [Int]
genInfPrime = gen 1
    where
        gen n
            | checkPrime n = n : gen (n + 1) --makes a list of the numbers that return true in checkPrime
            | otherwise = gen (n + 1) --otherwise go to next number

genPrimeTuples :: [(Int, Int)]
genPrimeTuples = zip genInfPrime (tail genInfPrime)

genPrimeDiff4 :: [(Int, Int)]
genPrimeDiff4 = [(x, x + 4) | x <- genInfPrime, checkPrime (x + 4)]
-- checks if x+4 is also a prime number, if yes then add the tuple
-- Maybe a little workaround because we dont exactly 'extract' the pairs, 
-- but rather just check if x + 4 is also a prime -> still works as asked

main :: IO()
main = do
    print (take 15 genInfPrime)
    print (take 15 genPrimeTuples)
    print (take 10 genPrimeDiff4)