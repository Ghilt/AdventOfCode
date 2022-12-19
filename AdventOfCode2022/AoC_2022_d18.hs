import Data.List
import Data.Char
import Data.List.Split (splitOn)
import Debug.Trace

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parse :: [String] -> (Int, Int, Int)
parse (x:y:z:[]) = (read x, read y,read z)

calculateSides :: [(Int, Int, Int)] -> Int
calculateSides = snd . foldl sumSides ([], 0)

sumSides :: ([(Int, Int, Int)], Int) -> (Int, Int, Int) -> ([(Int, Int, Int)], Int)
sumSides acc@(registeredCoords, currentSum) newCoord = (newCoord:registeredCoords, currentSum + sideCountDelta)
    where 
        adjacent = countAdjacent registeredCoords newCoord
        sideCountDelta = 6 - 2 * adjacent 

countAdjacent :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int
countAdjacent coords coord@(x,y,z) = count (\c -> elem c neighbors) coords
    where
        up    = (x,     y + 1, z    )
        down  = (x,     y - 1, z    )
        left  = (x - 1, y,     z    )
        right = (x + 1, y,     z    )
        front = (x,     y,     z + 1)
        back  = (x,     y,     z - 1)
        neighbors = [up, down, left, right, front, back]

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d18.txt"
    let coords = map (parse . splitOn ",") $ lines contents

    let exposedSides = calculateSides coords
    
    putStrLn $ "Result - Part 1: " ++ show (exposedSides)


----- Failed attempt att part2:

-- import Data.List
--import Data.Char
--import qualified Data.Set as Set
--import Data.List.Split (splitOn)
--import Debug.Trace
--
--count :: (a -> Bool) -> [a] -> Int
--count f = length . filter f
--
--parse :: [String] -> (Int, Int, Int)
--parse (x:y:z:[]) = (read x, read y,read z)
--
--
--calculateSidesPart1 :: [(Int, Int, Int)] -> Int
--calculateSidesPart1 = snd . foldl sumSidesPart1 ([], 0)
--
--sumSidesPart1 :: ([(Int, Int, Int)], Int) -> (Int, Int, Int) -> ([(Int, Int, Int)], Int)
--sumSidesPart1 acc@(registeredCoords, currentSum) newCoord = (newCoord:registeredCoords, currentSum + sideCountDelta)
--    where 
--        adjacent = countAdjacentPart1 registeredCoords newCoord
--        sideCountDelta = 6 - 2 * adjacent 
--
--countAdjacentPart1 :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int
--countAdjacentPart1 coords coord@(x,y,z) = count (\c -> elem c neighbors) coords
--    where
--        up    = (x,     y + 1, z    )
--        down  = (x,     y - 1, z    )
--        left  = (x - 1, y,     z    )
--        right = (x + 1, y,     z    )
--        front = (x,     y,     z + 1)
--        back  = (x,     y,     z - 1)
--        neighbors = [up, down, left, right, front, back]
--
----------------------
--
--calculateSides :: [(Int, Int, Int)] -> Int
--calculateSides = snd . foldl sumSides ([], 0)
--
--sumSides :: ([(Int, Int, Int)], Int) -> (Int, Int, Int) -> ([(Int, Int, Int)], Int)
--sumSides acc@(registeredCoords, currentSum) newCoord = {-(trace ("________ " ++ show newCoord)) $-} (newCoord:registeredCoords, currentSum + countSideDelta registeredCoords newCoord)
--
--countSideDelta :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int
--countSideDelta coords coord = {-(trace (show coord ++ show (map (isOutside (coord:coords)) $ getNeighbors coord))) $-}  sum $ map (isOutside (coord:coords)) $ getNeighbors coord -- it should be 0 if there is only negatives! right adding stuff in a cavity shouldnt count
--
--isOutside :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int -- +1 if outside, -1 if adjacent, -X whatever inner cavity size was that was capped
--isOutside environ n
--    | elem n environ = -1
--    | otherwise = {-(trace ("|> " ++ show n ++ show (environ))) $-} fillToOutside (Set.fromList []) [n] environ
--
--fillToOutside :: Set.Set (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
--fillToOutside seen [] environ =  {-(trace ("-TACE---> " ++ show (calculateSidesPart1 $ Set.toList seen))) $ -} 1 - (calculateSidesPart1 $ Set.toList seen)
--fillToOutside seen candidates@(x:xs) environ
--    | Set.size seen > 5000 = 1
--    | x `elem` environ = {-(trace ("-env---> " ++ show x)) $ -} fillToOutside seen xs environ 
--    | x `Set.member` seen = {-(trace ("-see----> " ++ show x)) $-}  fillToOutside seen xs environ 
--    | otherwise = {-(trace ("-----> " ++ show x)) $-}  fillToOutside newSeen (xs ++ (getNeighbors x)) environ
--    where
--        newSeen = Set.insert x seen 
--
--
--getNeighbors :: (Int, Int, Int) -> [(Int, Int, Int)] 
--getNeighbors (x, y, z) = [up, down, left, right, front, back]
--    where
--        up    = (x,     y + 1, z    )
--        down  = (x,     y - 1, z    )
--        left  = (x - 1, y,     z    )
--        right = (x + 1, y,     z    )
--        front = (x,     y,     z + 1)
--        back  = (x,     y,     z - 1)
--
--main = do  
--    putStrLn "Start..."   
--    contents <- readFile "input/AoC_2022_d18.txt"
--    let coords = map (parse . splitOn ",") $ lines contents
--
--    let exposedSides = calculateSides coords
--    
--    putStrLn $ "Result - Part 1: " ++ show (exposedSides)
--
--    let test = Set.fromList [1,2,3]
--    let test2 = Set.insert 4 test 
--    putStrLn $ "Result - Part 1: " ++ show (test)
--    putStrLn $ "Result - Part 1: " ++ show (test2)

