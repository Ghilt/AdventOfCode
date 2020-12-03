-- https://wiki.haskell.org/Import
import Data.Sequence hiding (length, filter)
import qualified Data.Sequence as Sequence
import Data.Foldable (length)

count :: Eq a => a -> Seq a -> Int
count x = Sequence.length . Sequence.filter (x==)

countTree :: Int -> Int -> Int -> String -> Int
countTree down right index row = count symbol yIsValid
     where
       symbol = row !! (x `mod` length row)
       yIsValid = index `mod` down
       x = right * index `div` down
       count '#' 0 = 1
       count  _  _ = 0

countTrees :: Seq String -> Int -> Int -> Int
countTrees treeRows down right = sum $ mapWithIndex (countTree down right) treeRows

main = do
    contents <- readFile "input/AoC_2020_d3.txt"
    let linesOfFile = fromList . lines $ contents
    let treeCountPart1 = count '#' $ mapWithIndex (\i row -> row !! ((i * 3) `mod` (length row))) linesOfFile
    putStrLn $ "Result - Part 1: " ++ show treeCountPart1

    -- Part 2
    let withSlope = countTrees linesOfFile
    let result = product [withSlope 1 1, withSlope 1 3, withSlope 1 5, withSlope 1 7, withSlope 2 1]

    putStrLn $ "Result - Part 2: " ++ show result