import Data.Char
import Data.Bits

getAsciiChars :: [Int] -> [Char]
getAsciiChars x = map chr x

uncover :: [Int] -> [Int] -> [Int] -> [Int]
uncover (x:xs) (y:ys) (z:zs)
    | xiThrow == yiThrow = uncover xs ys zs
    | otherwise = (zi * 25 + yi * 5 + xi * 1) : uncover xs ys zs
    -- I had to swap xi and zi for some reason, it did not work for xi * 25
        where 
            xi = x .&. 0x0F --get lower 4 bits with bit mask
            yi = y .&. 0x0F 
            zi = z .&. 0x0F
            xiThrow = x .&. 0xF0
            yiThrow = y .&. 0xF0
uncover _ _ _ = [] -- handle empty case

main :: IO()
main = do 
    numbersList <- readFile "encrypted_lists.txt"
    let [xList, yList, zList] = lines numbersList
    let x = read xList :: [Int]
    let y = read yList :: [Int]
    let z = read zList :: [Int] 
    let encryptedMessage = getAsciiChars (uncover x y z)
    putStrLn encryptedMessage