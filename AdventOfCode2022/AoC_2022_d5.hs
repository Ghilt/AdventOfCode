import Data.List
import Data.List.Split
import Debug.Trace -- This is just a good thing to not forget
import GHC.Utils.Misc
import Data.Char

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

isDigitOrSpace :: Char -> Bool
isDigitOrSpace c = c == ' ' || isDigit c

toTriple :: String -> (Int, Int, Int)
toTriple xs = (s !! 0, -1 +  s !! 1, -1 + s !! 2) -- Hehe whatever, this'll do
    where s = map read $ filter (not . null) $ splitOn " " $ filter isDigitOrSpace xs :: [Int]

execute :: [String] -> (Int, Int, Int) -> [String]
execute conf (1, from, to) = mapInd (moveSingle conf from to) conf
execute conf (amount, from, to) = execute (mapInd (moveSingle conf from to) conf) (amount - 1, from, to)

execute_2 :: [String] -> (Int, Int, Int) -> [String]
execute_2 conf (amount, from, to) = mapInd moveMulti conf
    where
       moveMulti stack index
        | index == from = drop amount stack
        | index == to = (take amount $ conf !! from) ++ stack
        | otherwise = stack     

moveSingle :: [String] -> Int -> Int -> String -> Int -> String
moveSingle conf from to stack index
    | index == from = tail stack
    | index == to = (head $ conf !! from):stack
    | otherwise = stack

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d5.txt"
    let linesOfFile = lines contents
    -- let numberOfBuckets = 1 + (length $ head linesOfFile) `div` 4

    let inputParts = splitOn [""] linesOfFile 
    let configuration = map (dropWhile isSpace) $ filter (any isAlpha) $ transpose $ init $ head $ inputParts
    let moves = map toTriple $ last $ inputParts

    let executedMoves = foldl execute configuration moves
    let executedMoves_2 = foldl execute_2 configuration moves

    putStrLn $ "Result - Part 1: " ++ show (executedMoves)
    putStrLn $ "Result - Part 2: " ++ show (executedMoves_2)
