import Data.List
import Data.List.Split (splitOn)

countCard :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
countCard acc (wins, index, _) = copyCardsInTheList index wins numberOfCopies acc
    where (_, _, numberOfCopies) = acc !! index

copyCardsInTheList :: Int -> Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
copyCardsInTheList startingAtIndex rangeToCopy copiesToMake = map (\(unChanged, index, amount) ->
            if index > startingAtIndex && index <= startingAtIndex + rangeToCopy
            then (unChanged, index, amount + copiesToMake)
            else (unChanged, index, amount)
        )

main :: IO ()
main = do
    putStrLn "Start..."
    contents <- readFile "input/AoC_2023_d4.txt"
    let input = lines contents
        refined = map (map words . splitOn "| " . drop 8) input
        winningNumbers = map (\xs -> filter (\ourNumber -> ourNumber `elem` head xs) $ last xs) refined
        totalPoints = sum . map (\x -> 2 ^ (length x - 1)) $ filter (not . null) winningNumbers

        -- Part 2
        indexed = zip3 (map length winningNumbers) [0..] $ repeat 1
        countCards = sum . map (\(_, _, amountOfCopies) -> amountOfCopies) $ foldl' countCard indexed indexed

    putStrLn $ "Result - Part 1: " ++ show totalPoints
    putStrLn $ "Result - Part 1: " ++ show countCards





