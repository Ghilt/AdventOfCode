import Data.Char

hasCountOf :: Int -> [Char] -> Bool
hasCountOf amount [] = False
hasCountOf amount (x:xs)
    | amountOfLetter > threshold = findAmount $ filter (/=x) xs
    | amountOfLetter == threshold = True
    | otherwise = findAmount xs
        where amountOfLetter = length $ filter (==x) xs
              threshold = amount - 1
              findAmount = hasCountOf amount

getClosenessOfWords :: [Char] -> [Char] -> Int
getClosenessOfWords w1 w2 = helper 0 w1 w2
    where helper differences w1 [] = differences
          helper differences (x:xs) (y:ys)
            | x == y = helper differences xs ys
            | x /= y = helper (differences + 1) xs ys

findClosestWord :: [[Char]] -> ([Char], [Char])
findClosestWord = helper (100, [], []) 
    where helper (_, w1, w2) [] = (w1, w2)
          helper result (line:theRest) = helper getResult theRest
              where accumulator = acc line
                    getResult = foldl accumulator result theRest

acc :: [Char] -> (Int, [Char], [Char]) -> [Char] -> (Int, [Char], [Char])
acc contender (best, w1, w2) next
        | diffs < best = (diffs, contender, next)
        | otherwise = (best, w1, w2)
            where diffs = getClosenessOfWords contender next

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d2.txt"
    let linesOfFile = lines contents
    let amountOfDoubles = length $ filter (hasCountOf 2) linesOfFile
    let amountOfTriplets = length $ filter (hasCountOf 3) linesOfFile
    let (word1, word2) = findClosestWord linesOfFile
    let na = '_'
    let commonLetters = filter (/=na) $ zipWith (\l1 l2 -> if l1 == l2 then l1 else na) word1 word2
    putStrLn $ "Amount of doubles: " ++ show amountOfDoubles
    putStrLn $ "Amount of triplets: " ++ show amountOfTriplets
    putStrLn $ "Checksum: " ++ show (amountOfDoubles * amountOfTriplets)
    putStrLn $ "Closest words: " ++ show (findClosestWord linesOfFile)
    putStrLn $ "Common letters: " ++ commonLetters