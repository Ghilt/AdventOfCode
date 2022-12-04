import Data.List.Split
import Data.List

toPair :: String -> (Char, Char)
toPair (m1:_:'X':[]) = (m1, 'A')
toPair (m1:_:'Y':[]) = (m1, 'B')
toPair (m1:_:'Z':[]) = (m1, 'C')

rps :: Char -> Char -> Int
rps 'A' 'C' = 6
rps 'B' 'A' = 6
rps 'C' 'B' = 6
rps m1 m2 
    | m1 == m2 = 3
    | otherwise = 0

moveValue :: Char -> Int
moveValue 'A' = 1
moveValue 'B' = 2
moveValue 'C' = 3

scoreIt :: (Char -> Int) -> (Char -> Char -> Int) -> (Char, Char) -> Int -> Int
scoreIt moveEvaluator interactionEvaluator (m1, m2) acc = acc + moveEvaluator m2 + interactionEvaluator m2 m1

reverseRps :: Char -> Char -> Int
reverseRps 'A' 'A' = 3
reverseRps 'A' 'B' = 1
reverseRps 'A' 'C' = 2
reverseRps 'B' m = moveValue m
reverseRps 'C' 'A' = 2
reverseRps 'C' 'B' = 3
reverseRps 'C' 'C' = 1

moveValue_2 :: Char -> Int
moveValue_2 'A' = 0
moveValue_2 'B' = 3
moveValue_2 'C' = 6


main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d2.txt"

    let calculateResults scorer = foldr scorer 0 . map toPair . lines 

    putStrLn $ "Result - Part 1: " ++ show (calculateResults (scoreIt moveValue rps) contents)
    putStrLn $ "Result - Part 1: " ++ show (calculateResults (scoreIt moveValue_2 reverseRps) contents)
