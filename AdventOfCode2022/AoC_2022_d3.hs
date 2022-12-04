import Data.List
import Data.List.Split
import Data.Char (ord)
import Debug.Trace

toPair :: String -> ([Char], [Char])
toPair backpack = splitAt (length backpack `div` 2) backpack

charPrio :: Char -> Int
charPrio c = let ascii = ord c in
    if ascii > 96
    then ascii - 96
    else ascii - 38

priorityValue :: ([Char], [Char]) -> Int
priorityValue (a, b) = 
    let c = intersect a b !! 0 in charPrio c

priorityValueOfGroup :: [String] -> Int
priorityValueOfGroup (a:b:c:[]) = charPrio ((intersect a $ intersect b c) !! 0)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d3.txt"
    let linesOfFile = lines contents

    let calculateResults_1 = sum . map priorityValue . map toPair
    let calculateResults_2 = sum . map priorityValueOfGroup . chunksOf 3 

    putStrLn $ "Result - Part 1: " ++ show (calculateResults_1 linesOfFile)
    putStrLn $ "Result - Part 2: " ++ show (calculateResults_2 linesOfFile)

