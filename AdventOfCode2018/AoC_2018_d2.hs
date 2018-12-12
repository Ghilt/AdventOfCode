import Data.Char

hasCountOf :: Int -> [Char] -> Bool
hasCountOf amount [] = False
hasCountOf amount (x:xs)
    | amountOfLetter > threshold = findAmount $ filter (/=x) xs
    | amountOfLetter == threshold = True
    | otherwise = findAmount xs
        where amountOfLetter = length (filter (==x) xs) 
              threshold = amount - 1
              findAmount = hasCountOf amount

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d2.txt"
    let linesOfFile = lines contents
    let amountOfDoubles = length $ filter (hasCountOf 2) linesOfFile
    let amountOfTriplets = length $ filter (hasCountOf 3) linesOfFile
    putStrLn $ "Amount of doubles: " ++ show amountOfDoubles
    putStrLn $ "Amount of triplets: " ++ show amountOfTriplets
    putStrLn $ "Checksum: " ++ show (amountOfDoubles * amountOfTriplets)