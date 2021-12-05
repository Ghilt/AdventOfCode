import Data.List
import Data.List.Split (splitOn, splitWhen)

parse :: [String] -> ([Int], [[[Int]]])
parse rows = (map read (splitOn "," $ head rows) :: [Int], splitWhen null toInts)
  where isolatedMatrixData = tail $ map (words) $ tail rows
        toInts = map (map read) isolatedMatrixData :: [[Int]]

-- This is just a trick to allow marking to be (*-1), get rid of the zeroes by shifting things +1, this has to be undone when winner is found
enableMarking :: ([Int], [[[Int]]]) -> ([Int], [[[Int]]])
enableMarking (numbers, boards) = (map (+1) numbers, map (map (map (+1))) boards)

thereIsBingo :: [[[Int]]] -> Bool
thereIsBingo boards = any (== True) $ map checkIfBingo boards

checkIfBingo :: [[Int]] -> Bool
checkIfBingo m = let check x = (any (all (<0)) x) in check m || (check $ transpose m)

mark :: Int -> [[[Int]]] -> [[[Int]]]
mark t boards = map (map (map markIt)) boards
  where markIt x = if x == t then -x else x 

play :: [Int] -> [[[Int]]] -> (Int, [[Int]])
play (drawnNumber:remainingNumbers) boards
  | thereIsBingo marked = (drawnNumber, head $ filter checkIfBingo marked)
  | otherwise = play remainingNumbers marked
    where marked = mark drawnNumber boards

-- More thoughts about the '.' function composition: So the last function happens first and feeds it's result to the the previous, backwards. 
sandbag :: [Int] -> [[[Int]]] -> (Int, [[Int]]) -> (Int, [[Int]])
sandbag (drawnNumber:remainingNumbers) boards currentLastWin
  | thereIsBingo marked = sandbag remainingNumbers (filter (not . checkIfBingo) marked) (drawnNumber, head $ filter checkIfBingo marked)
  | otherwise = sandbag remainingNumbers marked currentLastWin
    where marked = mark drawnNumber boards
sandbag [] boards currentLastWin = currentLastWin

computeResult :: (Int, [[Int]]) -> Int
computeResult (winningNumber, board) = (winningNumber - 1) * sum (map (subtract 1) $ filter (>0) $ concat board)

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2021_d4.txt"
    let rowsOfFile = lines contents
    let parsed = parse rowsOfFile
    let (numbers, boards) = enableMarking parsed
    let result1 = computeResult (play numbers boards)
    let result2 = computeResult (sandbag numbers boards (0, []))

    putStrLn $ "Result - Part 1: " ++ show (result1)
    putStrLn $ "Result - Part 1: " ++ show (result2)

