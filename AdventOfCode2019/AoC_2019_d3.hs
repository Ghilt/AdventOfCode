import Data.List.Split (splitOn)
import Data.Set (Set, fromList, intersection, toList)

foldRelativeIntoAbsolute :: [(Int, Int)] -> [String] -> [(Int, Int)]
foldRelativeIntoAbsolute coords [] = coords
foldRelativeIntoAbsolute [] (x:xs) = foldRelativeIntoAbsolute (expandToAbsPath (0,0) x) xs
foldRelativeIntoAbsolute coords@(previous:cs) (x:xs) = foldRelativeIntoAbsolute ((expandToAbsPath previous x) ++ coords) xs

expandToAbsPath :: (Int, Int) -> String -> [(Int, Int)] -- This function duplicates coords of the turning points, unfortunate, but doesn't matter if I convert to set later
expandToAbsPath coord@(x,y) command = let target@(tx,ty) = convertToAbs coord command in [(i,j) | i <- genListAtoB tx x, j <- genListAtoB ty y ]

convertToAbs :: (Int, Int) -> String -> (Int, Int)
convertToAbs (x,y) ('R':r) = (x + read r, y)    
convertToAbs (x,y) ('L':r) = (x - read r, y)
convertToAbs (x,y) ('U':r) = (x, y + read r)    
convertToAbs (x,y) ('D':r) = (x, y - read r)    

genListAtoB :: Int -> Int -> [Int] -- Doesn't care if decrementing och incrementing https://stackoverflow.com/questions/6806455/decrementing-ranges-in-haskell
genListAtoB a b | a == b = [a] -- If a == b this prevents an infinite list being generated
                | otherwise = [a, a + (signum $ b - a)..b]

getIntersection :: [(Int, Int)] -> [(Int, Int)] -> Set (Int, Int)
getIntersection a b = intersection (fromList a) (fromList b)

calculateManhattan ::  [(Int, Int)] ->  [Int]
calculateManhattan ls = let manhattan (x, y) = abs x + (abs y) in map manhattan ls

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2019_d3.txt"
    let commands = map (splitOn ",") $ lines contents
    let paths = map (foldRelativeIntoAbsolute []) commands 
    let intersection = getIntersection (head paths) (last paths)
    let intersections = calculateManhattan $ filter (/=(0,0)) $ toList intersection
    let closest = minimum intersections
    putStrLn $ "Part 1: " ++ show closest
