multiplicationTable :: [[Int]]
multiplicationTable = [
    [0,1,2,3,4,5,6,7,8,9],
    [1,2,3,4,0,6,7,8,9,5],
    [2,3,4,0,1,7,8,9,5,6],
    [3,4,0,1,2,8,9,5,6,7],
    [4,0,1,2,3,9,5,6,7,8],
    [5,9,8,7,6,0,4,3,2,1],
    [6,5,9,8,7,1,0,4,3,2],
    [7,6,5,9,8,2,1,0,4,3],
    [8,7,6,5,9,3,2,1,0,4],
    [9,8,7,6,5,4,3,2,1,0]
    ]

permutationTable :: [[Int]]
permutationTable = [
    [0,1,2,3,4,5,6,7,8,9],
    [1,5,7,6,2,8,3,0,9,4],
    [2,8,6,0,7,3,1,5,4,9],
    [3,9,8,7,6,0,5,2,1,4],
    [4,0,9,8,7,1,6,3,2,5],
    [5,1,0,9,8,2,7,4,3,6],
    [6,2,1,0,9,3,8,5,4,7],
    [7,3,2,1,0,4,9,6,5,8]
    ]

inverseTable :: [Int]
inverseTable = [0, 4, 3, 2, 1, 5, 6, 7, 8, 9]

checkDigits :: [Int] -> Bool
checkDigits digits
-- did not understand task, reasearched and also used chatgpt but couldnt get an understanding