import Data.List

bisect :: [Int] -> Char -> [Int]
bisect range c = case c of
   'F' -> firstHalf take
   'L' -> firstHalf take
   'B' -> firstHalf drop
   'R' -> firstHalf drop
  where firstHalf f = f (length range `div` 2) range

firstWithoutNeighbour :: [Int] -> Int
firstWithoutNeighbour (a:b:xs) = if b == a + 2 then a + 1 else firstWithoutNeighbour (b:xs)

main = do
    contents <- readFile "input/AoC_2020_d5.txt"
    let linesOfFile = lines contents

    let rows    = concatMap (foldl bisect [0..127]) $ map (take 7) linesOfFile
    let columns = concatMap (foldl bisect [0..7])   $ map (drop 7) linesOfFile
  
    let ids = zipWith (\row column -> row * 8 + column) rows columns
    let highestId = maximum ids

    let myId = firstWithoutNeighbour $ sort ids

    putStrLn $ "Result - Part 1: " ++ show (highestId)
    putStrLn $ "Result - Part 2: " ++ show (myId)
