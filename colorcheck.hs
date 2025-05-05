main :: IO ()
main = do
    let s = "Hello, Colourful World!"
        coloredRed = "Red: " ++ "\ESC[31m" ++ s ++ "\ESC[0m"  -- \ESC is the escape character
        coloredBlue = "Blue: " ++ "\ESC[34m" ++ s ++ "\ESC[0m"
    putStrLn coloredRed
    putStrLn coloredBlue