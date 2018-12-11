
getSides :: String -> [Int]
getSides a = map read (words a)
isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] = ((a + b) > c) && ((a + c) > b) && ((b + c) > a) 
countTriangles :: [[Int]] -> Int
countTriangles a = length (filter isValidTriangle a)

extractSides :: [[Int]] -> [[Int]]
extractSides [[a1,b1,c1],[a2,b2,c2],[a3,b3,c3]] = [[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]]
extractSides a = extractSides([a !! 0, a !! 1, a !! 2]) ++ extractSides(snd (splitAt 3 a)) 

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/day3.txt" 
    let linesOfFile = lines contents
    let listOfSides = map getSides linesOfFile
    putStrLn $ "Amount of valid triangles problem 1: " ++ show(countTriangles listOfSides)
    putStrLn $ "Amount of valid triangles problem 2: " ++ show(countTriangles $ extractSides listOfSides)