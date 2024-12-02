import Data.List (sort)
import qualified Data.Set as Set

noDuplicates :: [Int] -> Bool
noDuplicates xs = length xs == Set.size (Set.fromList xs)

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = xs == ascending || xs == descending
  where
    ascending  = sort xs
    descending = reverse (sort xs)

negativeIfTooLargeGap :: [Int] -> Int
negativeIfTooLargeGap = foldl1 (\acc x -> 
    let diff = abs(acc - x) 
    in case () of
        _ | acc < 0         -> acc
          | diff > 3        -> -x
          | otherwise       -> x
    )

stupidlyExpandIt :: [Int] -> [[Int]]
stupidlyExpandIt xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/aoc_2024_d2.txt"
    let input = lines contents
        asInts = map (map read . words) input :: [[Int]]
        onlyAscDesc = filter (\x -> isSorted x && noDuplicates x)
        onlySafe ad = length $ filter (>0) $ map negativeIfTooLargeGap ad
        part1 ad = onlySafe $ onlyAscDesc ad

        expandItStupidly = map stupidlyExpandIt asInts
        thenJustDoPart1More = length $ filter (>0) $  map part1 expandItStupidly

    putStrLn $ "Result - Part 1: " ++ show (part1 asInts)
    putStrLn $ "Result - Part 2: " ++ show thenJustDoPart1More

