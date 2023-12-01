{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Text (Text, replace, pack, unpack)
import Data.Char
import Debug.Trace -- I will do some tracing just so i don't forget that it exists

replaceSubstrings :: [(Text, Text)] -> Text -> Text
replaceSubstrings replacements originalText =
  foldl (\text (old, new) -> replace old new text) originalText replacements

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2023_d1.txt"
    let input = lines contents
        replacements = [ ("one", "o1e"), ("two", "t2o"), ("three", "t3e"),("four", "f4r"), ("five", "f5e"), ("six", "s6x"),("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e")]
        inputDigified = map (unpack . replaceSubstrings replacements . pack) input
        calculate = sum . map (\line -> let c = map digitToInt $ filter isDigit line in 10 * head c + last c)
        part1 = calculate input
        part2 = calculate inputDigified

    putStrLn $ "Result - Part 1: " ++ show part1
    putStrLn $ "Result - Part 2: " ++ show part2
