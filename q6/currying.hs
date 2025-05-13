foo :: Bool -> Int -> Int -> Int
foo = \x -> \y -> \z -> 
    if x then y + z
    else y * z 

main :: IO()
main = do
    print (foo False 10 6)
    