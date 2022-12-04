import Data.List
import Data.List.Split
import Debug.Trace -- This is just a good thing to not forget
import GHC.Utils.Misc

parse :: String -> ((Int, Int),(Int, Int))
parse xs = asInt
    where 
        pairs = (map (splitOn "-") . splitOn ",") xs
        asInt = last2 $ map (last2 . map read) pairs :: ((Int, Int),(Int, Int))

pairContainsOther :: (Int, Int) -> (Int, Int) -> Bool
pairContainsOther (lA, hA) (lB, hB) = lA <= lB && hA >= hB

pairsOverlap :: (Int, Int) -> (Int, Int) -> Bool
pairsOverlap (lA, hA) (lB, hB) = (hA >= lB && lA <= hB)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d4.txt"
    let linesOfFile = lines contents
    let parsed = map parse linesOfFile
    let containsInfo = length . filter (\b -> b) . map (\(e1,e2) -> pairContainsOther e1 e2 || pairContainsOther e2 e1)
    let overlapInfo = length . filter (\b -> b) . map (\(e1,e2) -> pairsOverlap e1 e2)

    -- last2 function in GHC.Utils.Misc makes list into tuple, think I couldve used that a lot before

    -- Also haven't been good at realizing that you can push the read :: Int cast thing to the end with whatever type you require in the end

    putStrLn $ "Result - Part 1: " ++ show (containsInfo parsed)
    putStrLn $ "Result - Part 2: " ++ show (overlapInfo parsed)


