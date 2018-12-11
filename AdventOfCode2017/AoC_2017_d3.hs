import Data.List
import Data.Maybe

traceSpiral :: ((Int, Int), Char, (Float, Int), Int, Int) -> ((Int, Int), Char, (Float, Int), Int, Int)
traceSpiral (cord, dir, (segLen, segRem), counter, goal) 
               | counter == goal = (cord, dir, (segLen, segRem), counter, goal) 
               | otherwise = traceSpiral(move cord dir, turn segRem dir, changeSegment (segLen, segRem), counter + 1, goal)

changeSegment :: (Float, Int) -> (Float, Int)
changeSegment (segLen, segRem)
    | segRem == 0 = (segLen + 0.5, floor $ segLen + 0.5)
    | otherwise = (segLen, segRem - 1)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) dir
    | dir == 'E' = (x + 1, y)
    | dir == 'W' = (x - 1, y)
    | dir == 'N' = (x, y + 1)
    | dir == 'S' = (x, y - 1)
    | otherwise = (x,y )

turn :: Int -> Char -> Char
turn segRem dir
    | segRem /= 0 = dir
    | dir == 'N' = 'W'
    | dir == 'S' = 'E'
    | dir == 'E' = 'N'
    | dir == 'W' = 'S'
    
main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2017_d3.txt" 
    let number = read contents :: Int
    let start = ((0, 0), 'N', (0, 0), 1, number)
    let ((x, y), _, _, _, _) = traceSpiral start
    let part1 = abs x + abs y
    putStrLn $ "coord 1: " ++ show part1
