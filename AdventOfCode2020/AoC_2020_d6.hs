import Data.List.Unique
import Data.Set as Set hiding (foldl, map)

groupItems :: String -> [String] -> String -> [String]
groupItems separator xs "" = "":xs
groupItems separator (x:xs) newData = (newData ++ separator ++ x):xs

commonOccurance :: Set Char -> Set Char -> Set Char
commonOccurance acc item = acc `intersection` item 

main = do
    contents <- readFile "input/AoC_2020_d6.txt"
    let linesOfFile = lines contents
    let grouped = foldl (groupItems "") [""] linesOfFile
    let countUnique = sum $ map (length . count) grouped

    -- Part 2
    let groupedAsSets = map (map Set.fromList . words) $ foldl (groupItems " ") [""] linesOfFile

    let commonOccurances = sum $ map (length . foldl1 commonOccurance) groupedAsSets

    putStrLn $ "Result - Part 1: " ++ show countUnique
    putStrLn $ "Result - Part 2: " ++ show commonOccurances
