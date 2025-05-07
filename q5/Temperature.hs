ownMap :: (a -> b) -> [a] -> [b]
ownMap func (x:xs) = func x : ownMap func xs --recursion to always apply func to head
ownMap _ [] = []

ownFilter :: (a -> Bool) -> [a] -> [a]
ownFilter func (x:xs)
    | func x = x : ownFilter func xs
    | otherwise = ownFilter func xs
ownFilter _ [] = []
-- if func x is true then add it to list, otherwise skip it

avgTemp :: (Fractional a) => (a, a) -> a
avgTemp (high, low) = (high + low) / 2

classifyTemp :: (Fractional a, Ord a) => [a] -> [String]
classifyTemp [] = []
classifyTemp (x:xs)
    | x < 10 = "cold" : classifyTemp xs
    | x > 25 = "hot" : classifyTemp xs 
    | otherwise = "moderate" : classifyTemp xs

main :: IO()
main = do
    let temps = [(32,23), (12, 5), (17,10), (29,20), (25,15),
                (10,3), (15, 8), (31,19), (35,27), (22,12)]
    print (ownMap avgTemp temps)

    let avgTempList = ownMap avgTemp temps
    print (classifyTemp avgTempList)

    let classifiedList = classifyTemp avgTempList
    let filterHot = ownFilter (== "hot") classifiedList 

    if length filterHot > (length avgTempList) `div` 2 
        then print "Heatwave!"
        else print "No heat wave :)"


