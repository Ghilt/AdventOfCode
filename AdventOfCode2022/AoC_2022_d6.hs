import Data.List
import Data.List.Unique
import qualified Data.Text as Teeckst
import Data.Text.Internal.Search (indices)

-- This was an old version of windowed i had, made a new better one with window size
--windowed :: [a] -> [[a]]
--windowed list = slide [] list 
--    where
--        slide result (a:b:c:d:list) = slide (result ++ [[a,b,c,d]]) (b:c:d:list)
--        slide result tooSmall = result

-- How can haskell not have a built in for windowed?
windowed :: Int -> [a] -> [[a]]
windowed size list = slide [] list 
    where
        slide result list
            | length list >= size = slide (result ++ [take size list]) (tail list)
            | otherwise = result

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d6.txt"

    let marker4Char  =  4 + (length $ takeWhile (not . allUnique) $ windowed 4 contents)
    let marker14Char = 14 + (length $ takeWhile (not . allUnique) $ windowed 14 contents)

    putStrLn $ "Result - Part 1: " ++ show (marker4Char)
    putStrLn $ "Result - Part 2: " ++ show (marker14Char)

