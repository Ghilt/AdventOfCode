import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)
import Debug.Trace

-- Function to extract pairs of numbers
extractMulPairs :: String -> [(Int, Int)]
extractMulPairs input = 
    -- Find all matches of the regex in the input string
    let matches = input =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [[String]]
    in mapMaybe toPair matches
  where
    -- Convert regex match groups into a pair of Ints
    toPair :: [String] -> Maybe (Int, Int)
    toPair (_:x:y:_) = Just (read x, read y)
    toPair _         = Nothing

sumOfProducts :: [(Int, Int)] -> Int
sumOfProducts = sum . map (uncurry (*))

resolveDosAndDonts :: String -> String
resolveDosAndDonts input =
    let (storedStringAcc, _, _) = foldl processChar ("", True, "_____") input
    in storedStringAcc
  where
    processChar :: (String, Bool, String) -> Char -> (String, Bool, String)
    processChar (acc, flag, latest) c =
        let newLatest = tail latest ++ [c]
            newFlag = {-trace ("z = " ++ show (latest ++ " flip = " ++ show flag)) $ -} case newLatest of
                        "don't"               -> False
                        [_, _, _, 'd', 'o']   -> True
                        _                     -> flag    
            newAcc = if flag then acc ++ [c] else acc
        in (newAcc, newFlag, newLatest)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/aoc_2024_d3.txt"
    let input = concat $ lines contents
        part1 = sumOfProducts $ extractMulPairs input
        part2 = sumOfProducts $ (extractMulPairs . resolveDosAndDonts) input

    putStrLn $ "Result - Part 1: " ++ show part1
    putStrLn $ "Result - Part 2: " ++ show part2


