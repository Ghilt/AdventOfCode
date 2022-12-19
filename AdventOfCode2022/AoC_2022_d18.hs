import Data.Char
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Debug.Trace
import Data.Time

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parse :: [String] -> (Int, Int, Int)
parse (x:y:z:[]) = (read x, read y,read z)

calculateSides :: [(Int, Int, Int)] -> Int
calculateSides xs = snd . foldl sumSides (xs, 0) $ xs

sumSides :: ([(Int, Int, Int)], Int) -> (Int, Int, Int) -> ([(Int, Int, Int)], Int)
sumSides acc@(allCoords, currentSum) newCoord = (allCoords, currentSum + countSideDelta allCoords newCoord)

countSideDelta :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int
countSideDelta coords coord = sum $ map (isOutside coords) $ getNeighbors coord

isOutside :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int 
isOutside environ n
    | elem n environ = 0
    | otherwise = fillToOutside (Set.fromList []) [n] environ

fillToOutside :: Set.Set (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
fillToOutside seen [] environ = 0
fillToOutside seen (c:cs) environ
    | candidateIsOutsideBoundingBox c = 1
    | c `elem` environ = fillToOutside seen cs environ 
    | c `Set.member` seen = fillToOutside seen cs environ 
    | otherwise = fillToOutside newSeen (cs ++ (getNeighbors c)) environ
    where
        newSeen = Set.insert c seen 
        candidateIsOutsideBoundingBox (x,y,z) = True -- Part 1
        --candidateIsOutsideBoundingBox (x,y,z) = x >= 20 || y >= 20 || z >= 20 -- Part 2, hardcoded bounding box based on input


getNeighbors :: (Int, Int, Int) -> [(Int, Int, Int)] 
getNeighbors (x, y, z) = [up, down, left, right, front, back]
    where
        up    = (x,     y + 1, z    )
        down  = (x,     y - 1, z    )
        left  = (x - 1, y,     z    )
        right = (x + 1, y,     z    )
        front = (x,     y,     z + 1)
        back  = (x,     y,     z - 1)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d18.txt"
    let coords = map (parse . splitOn ",") $ lines contents

    time <- getZonedTime
    print time -- Print time

    let exposedSides = calculateSides coords

    putStrLn $ "Result - Part 1 or 2: " ++ show (exposedSides) -- Shortcut, line for part 2 solution in comment above

    time <- getZonedTime
    print time


