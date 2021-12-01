import Data.List.Split

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

windowed :: [a] -> [[a]]
windowed list = slide [] list 
    where
        slide result (a:b:c:list) = slide (result ++ [[a,b,c]]) (b:c:list)
        slide result tooSmall = result


main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2021_d1.txt"
    let linesOfFile = map read (lines contents) :: [Int]
    let first = head linesOfFile
    let countIncrements = foldl (\(increments, lastValue) current -> (increments + boolToInt (current > lastValue), current)) (0, first) linesOfFile 
    let windows = windowed linesOfFile
    let findInc (increments, lastValue) current = let summed = sum current in (increments + boolToInt (summed > lastValue), summed)
    let countWindowIncrements = foldl findInc (0, sum $ head windows) windows

    putStrLn $ "Result - Part 1: " ++ show countIncrements
    putStrLn $ "Result - Part 2: " ++ show countWindowIncrements
