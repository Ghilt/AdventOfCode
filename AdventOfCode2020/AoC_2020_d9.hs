import Data.List

indexed :: [a] -> [(Int, a)]
indexed list = indexed0 0 list
    where indexed0 index (x:xs) = (index, x):(indexed0 (index + 1) xs)
          indexed0 index [] = []

preambledFilter :: Int -> ([a] -> Bool) -> [a] -> [a]
preambledFilter size f xs = filterIt [] size
   where filterIt acc index
            | length xs <= index = acc
            | otherwise = if f (take size . drop (index - size + 1) $ xs) 
                          then (filterIt ((xs !! index):acc) (index + 1)) 
                          else (filterIt acc (index + 1))

anyComboSums ::  [Int] -> Bool
anyComboSums xs = not $ (last xs) `elem` [ x+y | (x:rest) <- tails xs , y <- rest ]

addsUpToExactly :: Int -> [Int] -> Bool
addsUpToExactly target xs = 0 < length (filter (\x -> (sum x) == target) $ inits xs) 

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil item (x:xs)
    | item == x = []
    | otherwise = x:(takeUntil item xs)

main = do
    contents <- readFile "input/AoC_2020_d9.txt"
    let linesOfFile = map read (lines contents) :: [Int]
    let p1Result = head $ preambledFilter 26 anyComboSums linesOfFile

    let p2Region = head $ filter (addsUpToExactly p1Result) $ tails linesOfFile
    
    -- Meh ugly, but whatever
    let breakpoint = snd $ foldl (\(acc, target) x -> if (acc == target) then (acc, x) else (acc + x, target)) (0, p1Result) p2Region

    let contiguousSet = takeUntil breakpoint p2Region

    let p2Result = minimum contiguousSet + maximum contiguousSet

    putStrLn $ "Result - Part 1: " ++ show p1Result
    putStrLn $ "Result - Part 2: " ++ show p2Result
