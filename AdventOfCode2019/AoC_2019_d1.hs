
calculateAllFuel :: Int -> Int
calculateAllFuel v = accumulate 0 v
    where accumulate a b 
            | b > 0 = let moreFuel = calculateFuel b in accumulate (a + moreFuel) moreFuel
            | otherwise = a

calculateFuel :: Int -> Int
calculateFuel v
    | v > 5 = v `div` 3 - 2
    | otherwise = 0

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2019_d1.txt"
    let mass = map read $ lines contents
    let fuel = map calculateFuel mass
    let sumIt = foldl (+) 0 fuel
    putStrLn $ "Part 1 fuel: " ++ show sumIt
    
    let fuel2 = map calculateAllFuel mass
    let sumIt2 = foldl (+) 0 fuel2
    putStrLn $ "Paprt 2 fuel: " ++ show sumIt2