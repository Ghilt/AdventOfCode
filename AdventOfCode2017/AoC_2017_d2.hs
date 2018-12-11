import Data.List
import Data.Maybe

getMinMax :: (Int, Int) -> Int -> (Int, Int)
getMinMax (min, max) newValue 
    | newValue < min && newValue > max = (newValue, newValue)
    | newValue < min = (newValue, max)  
    | newValue > max = (min, newValue) 
    | otherwise = (min, max) 

sumDifference :: Int -> (Int, Int) -> Int
sumDifference accu (min, max) = accu + max - min

-- // Part 2 \\ --

questForDivisors :: [Int] -> (Int, Int)
questForDivisors list = (fromJust $ find (isDivisible foundIt) list, foundIt)
                            where foundIt = fromJust $ find (divisiblePredicate list) list

isDivisible :: Int -> Int -> Bool 
isDivisible n m 
    | n `mod` m == 0 && n /= m = True 
    |otherwise = False

divisiblePredicate :: [Int] -> Int -> Bool
divisiblePredicate list v = isJust $ find (isDivisible v) list

divideAndSum :: Int -> (Int, Int) -> Int
divideAndSum accu (divisor, val) = accu + (val `div` divisor)

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2017_d2.txt" 
    let linesOfFile = lines contents
    let listOfIntLists = map (map (read :: String -> Int)) $ map words linesOfFile
    let listOfMinMaxes = map (foldl getMinMax (maxBound :: Int, 0)) listOfIntLists
    let checkSum =  foldl sumDifference 0 listOfMinMaxes
    let findEm = map questForDivisors listOfIntLists
    let sumEm = foldl divideAndSum 0 findEm
    putStrLn $ "Sum for problem 1: " ++ show checkSum
    putStrLn $ "Sum for problem 2: " ++ show sumEm
