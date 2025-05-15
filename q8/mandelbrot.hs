import Data.Complex

mandelbrot :: Int -> Complex Double -> Complex Double -> [Complex Double]
mandelbrot i z0 c = take i $ iterate (\z -> z*z + c) z0

checkMandelbrot :: Complex Double -> Bool
checkMandelbrot c = magnitude (last $ mandelbrot 100 0 c) < 10
-- magnitude takes the absolute value of a complex number

row :: [Double] -> Double -> String
row xs y = concat [ if checkMandelbrot (x :+ y) then "\ESC[37m*\ESC[0m" else escapeColor (escapeCount (x :+ y)) | x <- xs ]
-- checks for each point in the xAxis if the corresponding complex number is in the set
-- concat needed due to list comprehenson returning a list of strings

escapeCount :: Complex Double -> Int
escapeCount c = loop 0 0
    where 
        loop i z
            | magnitude z > 1000 = i
            | otherwise = loop (i + 1) (z*z + c)


escapeColor :: Int -> String
escapeColor i 
    | i == 3 = "\ESC[36m.\ESC[0m"
    | i == 4 = "\ESC[34m.\ESC[0m"
    | i == 5 = "\ESC[36m.\ESC[0m"
    | i == 6 = "\ESC[34m.\ESC[0m"
    | i == 7 = "\ESC[36m.\ESC[0m"
    | i == 8 = "\ESC[34m.\ESC[0m"
    | i == 9 = "\ESC[36m.\ESC[0m"
    | i == 10 = "\ESC[34m.\ESC[0m"
    | otherwise = "\ESC[31m.\ESC[0m"

main :: IO ()
main = do
    let xAxis = [-2, -1.95 .. 1]  
    let yAxis = [1.5, 1.4 .. -1.5]  

    mapM_ (\y -> putStrLn (row xAxis y)) yAxis
-- maps the result of the printing function to a yAxis point
