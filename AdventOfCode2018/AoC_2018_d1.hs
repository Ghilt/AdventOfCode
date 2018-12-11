import Data.Char

-- Doing Haskell again for the first time in a long while. The multiple 'newNumber + (head accumulator)' should definately be written 
-- in a more clever way, maybe 'where'/'let' keywords but it's late and I can't recall the syntax
accumulateUntilFirst :: (Bool, [Int]) -> Int -> (Bool, [Int])
accumulateUntilFirst inValue@(finished, accumulator) newNumber
    | finished = inValue
    | elem (newNumber + (head accumulator)) accumulator = (True, [(newNumber + (head accumulator))]) 
    | otherwise = (False, (newNumber + (head accumulator)) : accumulator)            

recurrToFindDuplicateFrequency :: (Bool, [Int]) -> [Int] -> Int
recurrToFindDuplicateFrequency inValue@(finished, accumulator) frequencies
    | finished = head accumulator
    | otherwise = recurrToFindDuplicateFrequency (foldl accumulateUntilFirst inValue frequencies) frequencies

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d1.txt"
    let purgedContentsOfPlusSign = filter (/='+') contents
    let linesOfFile = lines purgedContentsOfPlusSign
    let numbers = map (read::String->Int) linesOfFile
    --let numbers_the_same_test = map read linesOfFile ::[Int]
    let sumIt = foldl (+) 0 numbers
    let findFirst = recurrToFindDuplicateFrequency (False, [0]) numbers
    putStrLn $ "Print first number in a roundabout educationary way " ++ show (read (head linesOfFile) :: Int)
    putStrLn $ "Sum all: " ++ show sumIt
    putStrLn $ "Find the first duplicate frequency: " ++ show findFirst