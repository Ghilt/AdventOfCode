import Data.List

-- transpose:: [[a]]->[[a]]
-- transpose ([]:_) = []
-- transpose x = (map head x) : transpose (map tail x)

toCountedList :: String -> [(Int,Char)]
toCountedList str = sort $ zip (map countAll str) str -- Very innefficient; counts all letter multiple times
    where countAll c = length $ filter (== c) str

--getLetter :: ([a] -> a) -> [(Int,Char)] -> Char --I want the signature to be more like this, but it doesn't work
--getLetter :: ([(Int,Char)] -> (Int,Char)) -> [(Int,Char)] -> Char -- This works
getLetter getter list = snd $ getter list

main = do  
    contents <- readFile "input/day6.txt" 
    let linesOfFile = lines contents
    let transposed = transpose linesOfFile
    let countedList = map toCountedList transposed

    putStrLn $ "Real transmission, most: " ++ let getMost = getLetter head in show (map getMost countedList)
    putStrLn $ "Real transmission, least: " ++ let getLeast = getLetter last in show (map getLeast countedList)

