import Data.List (transpose, sort)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/aoc_2024_d1.txt"
    let input = lines contents
        asInts = transpose $ map (map read . words) input :: [[Int]]
        sortedPairs = transpose $ map sort asInts
        part1 = sum $ map (\pair -> abs $ head pair - last pair) sortedPairs

        countInt i = length . filter (== i)
        part2 = sum $ map (\p -> let freq = last asInts in p * countInt p freq) (head asInts) 

    putStrLn $ "Result - Part 1: " ++ show part1
    putStrLn $ "Result - Part 2: " ++ show part2
