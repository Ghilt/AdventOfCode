import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (fromJust)

--https://en.wikipedia.org/wiki/A*_search_algorithm

createStartParameters :: (Int, Int) -> (Int, Int) -> ((Int, Int),
                                                        Set.Set (Int, Int), 
                                                        Set.Set (Int, Int), 
                                                        Map.Map (Int, Int) (Int, Int), 
                                                        Map.Map (Int, Int) Float, 
                                                        Map.Map (Int, Int) Float )
createStartParameters start goal = (goal,
                                    Set.empty, 
                                    Set.insert start Set.empty, 
                                    Map.empty, 
                                    Map.insert start 0 Map.empty, 
                                    Map.insert start (heuristicCostEstimate start goal) Map.empty)

input = 1352

isWall :: (Int, Int) -> Bool
isWall (x, y) = let n = x*x + 3*x + 2*x*y + y + y*y + input
                    binaryRep = showIntAtBase 2 intToDigit n "" 
                    sumOfBinaryOdd = foldl (\x y -> if (y == '1') then not x else x ) False binaryRep
                in sumOfBinaryOdd

isValid :: (Int, Int) -> Bool
isValid (x, y) = not (isWall (x, y)) && x >= 0 && y >= 0

getPossibleNeighbors :: (Int, Int) -> [(Int, Int)]
getPossibleNeighbors (x, y) =  filter isValid $ (x-1, y):(x+1 ,y):(x, y+1):(x, y-1):[]

heuristicCostEstimate :: (Int, Int) -> (Int, Int) -> Float
heuristicCostEstimate (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
                                             where x' = fromIntegral $ x1 - x2
                                                   y' = fromIntegral $  y1 - y2

reconstructPath :: (Int, Int) ->  Map.Map (Int, Int) (Int, Int) -> [(Int, Int)]
reconstructPath gg cameFrom = mkPath (gg:[])
    where mkPath path@(n:_) = case n2 of Nothing -> path
                                         Just next -> mkPath (next:path) 
                where n2 = Map.lookup n cameFrom

-- Goal, Closed Set, Open Set, cameFrom gScore, fScore
aStarAlgorithm ::(Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) Float -> Map.Map (Int, Int) Float -> [(Int, Int)]
aStarAlgorithm g cs os cf gs fs
    | current == g = reconstructPath current cf
    | Set.empty == os = []
    | otherwise = aStarAlgorithm g newCs nOs nCf nGs nFs
          where current = fst $ getbyFScore os fs
                newOs = Set.delete current os
                newCs = Set.insert current cs
                notAlreadyEvaluated x = not $ Set.member x newCs 
                tentativeGScore = let cGScore = Map.lookup current gs 
                                  in case cGScore of Nothing -> 99999999
                                                     Just a -> 1 + a
                isNewShorterNode x = let cGScore = Map.lookup x gs 
                                     in case cGScore of Nothing -> True
                                                        Just a -> tentativeGScore < a
                neighborsStep1 = filter notAlreadyEvaluated $ getPossibleNeighbors current
                neighborsStep2 = filter isNewShorterNode $ neighborsStep1
                evalNeighbor (tos, tcf, tgs, tfs) n = (Set.insert n tos, 
                                                       Map.insert n current tcf, 
                                                       Map.insert n tentativeGScore tgs, 
                                                       Map.insert n (heuristicCostEstimate n g + tentativeGScore) tfs)
                (nOs, nCf, nGs, nFs) = foldl evalNeighbor (newOs, cf, gs, fs) neighborsStep2

getbyFScore :: Set.Set (Int, Int) -> Map.Map (Int, Int) Float -> ((Int, Int), Float)
getbyFScore openSet = foldl findSmallest ((0,0), 9999999999) . Map.toList
    where findSmallest old@(_, vOld) contender@(point, v)
            | vOld > v && (Set.member point openSet) = contender
            | otherwise = old

explore50 target = length $ aStarAlgorithm target startCS startOS startCF startGS startFS
    where (goal, startCS, startOS, startCF, startGS, startFS) = createStartParameters (1,1) target

main = do
    putStrLn "Start..."  
    let (goal, startCS, startOS, startCF, startGS, startFS) = createStartParameters (1,1) (31,39)
        coords = filter (\ (x, y) -> x + y < 51) $ [ (x,y) | x<-[0..52], y<-[0..52] ]
        exhaustAlgo = map explore50 coords
        filteredSuccess = filter (\ x -> x /= 0 && x < 52) exhaustAlgo

    putStrLn $ "goal: " ++ show (goal)
   -- putStrLn $ "goal is wall: " ++ show (exhaustAlgo)
    putStrLn $ "StartFs : " ++ show (startFS)
    putStrLn $ "possible neighbors of 1,2 " ++ show (getPossibleNeighbors (1,2))
--    putStrLn $ "A* algorithm: " ++ show (aStarAlgorithm goal startCS startOS startCF startGS startFS)
    putStrLn $ "A* algorithm: " ++ show (length $ aStarAlgorithm goal startCS startOS startCF startGS startFS)
    putStrLn $ "goal is valid: " ++ show ( length filteredSuccess) --Attempt at solution 2, not accurate
