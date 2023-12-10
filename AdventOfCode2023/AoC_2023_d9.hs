
areAllZeros :: [Int] -> Bool
areAllZeros xs = all (== 0) xs

differences :: [Int] -> [Int]
differences (a:[]) = [0]
differences xs = map (\(a, b) -> b - a) $ zip xs (drop 1 xs) 

calculateSequence :: [[Int]] -> [[Int]] 
calculateSequence full@(xs:rest) 
    | areAllZeros xs = full
    | otherwise = calculateSequence $ (differences xs):full


main :: IO ()
main = do
    putStrLn "Start..."
    contents <- readFile "input/AoC_2023_d9.txt"
    let input = lines contents
        refined = map (map read . words) input :: [[Int]]
        sequences = map (calculateSequence . pure) refined
        part1 = sum $ concatMap (map last) sequences
        part2 = sum . map (foldl1 subtract) . map (map head) $ sequences

    putStrLn $ "Result - Part 1: " ++ show part1
    putStrLn $ "Result - Part 2: " ++ show part2





