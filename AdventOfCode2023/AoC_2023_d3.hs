{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.List.Split as Split
import Data.Char
import Debug.Trace -- I will do some tracing just so i don't forget that it exists

import qualified Data.Set as Set

isActivatorCharacterAt :: Int -> [Char] -> Bool
isActivatorCharacterAt i list
  | i < 0 || i >= length list = False
  | otherwise = list !! i /= '.'

foldlIndexed :: (b -> Int -> a -> b) -> b -> [a] -> b
foldlIndexed f initialValue list = foldl' (\acc (index, x) -> f acc index x) initialValue (zip [0..] list)

findActiveNumbers :: (String, String, String) -> [Int]
findActiveNumbers (a, b, c) = extractNumbers folded
  where folded = foldlIndexed accumulateState (baseAcc a c)  b
        extractNumbers (_, _, cs) = map fst $ filter snd cs

baseAcc :: String -> String -> (String, String, [(Int, Bool)])
baseAcc a b = (a, b, [(0, False)])

accumulateState :: (String, String, [(Int, Bool)]) -> Int -> Char -> (String, String, [(Int, Bool)])
accumulateState (a, b, c@(currentNumber, active):past) index '.' = (a, b, (0, False):c:past)
accumulateState (a, b, (currentNumber, active):past) index character
  | isDigit character = (a, b, (currentNumber * 10 + digitToInt character, active || becameActive):past)
  | currentNumber == 0 = (a, b, (currentNumber, True):past)
  | otherwise = (a, b, (0,True):(currentNumber, True):past)
    where dback = index - 1
          dforw = index + 1
          becameActive = isActivatorCharacterAt dback a
                      || isActivatorCharacterAt index a
                      || isActivatorCharacterAt dforw a
                      || isActivatorCharacterAt dback b
                      || isActivatorCharacterAt index b
                      || isActivatorCharacterAt dforw b

-- part 2

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = Set.toList . Set.fromList

extractNumber :: Int -> String -> String
extractNumber index str
  | index < 0 || index >= length str = "0"
  | not (isDigit (str !! index)) = "0"
  | otherwise = extractDigits index str

extractDigits :: Int -> String -> String
extractDigits index str = leftDigits ++ [digitAtIndex] ++ rightDigits
  where
    digitAtIndex = str !! index
    leftDigits = reverse (takeWhile isDigit (reverse (take index str)))
    rightDigits = takeWhile isDigit (drop (index + 1) str)

findDuoGears :: (String, String, String) -> [Int]
findDuoGears (a, b, c) = gearNumbers
  where (_, _, _, gearNumbers) = foldlIndexed accumulateStatePart2 (a, b, c, [0]) b

accumulateStatePart2 :: (String, String, String, [Int]) -> Int -> Char -> (String, String, String, [Int])
accumulateStatePart2 (a, b, c, past) i '*' = (a, b, c, findGearRatio:past)
    where back = i - 1
          forw = i + 1
          findGearRatio = calculateGearRatio [
            -- line above
            getNumberAt back a, 
            getNumberAt i a,
            getNumberAt forw a,
            -- same line
            getNumberAt back b,
            getNumberAt forw b,
            -- line below
            getNumberAt back c,
            getNumberAt i c,
            getNumberAt forw c
            ]
          
accumulateStatePart2 acc _ _ = acc

-- leaving in the trace example
-- trace ("Surrounding spots :" ++ show eightSurroundingSpots ++ " gave " ++ show result)
calculateGearRatio :: [Int] -> Int
calculateGearRatio eightSurroundingSpots = result
  where result = pruneSingleValueAndTakeProduct $ filter (/=0) $ removeDuplicates eightSurroundingSpots
        pruneSingleValueAndTakeProduct [x] = 0
        pruneSingleValueAndTakeProduct xs = product xs

getNumberAt :: Int -> String -> Int
getNumberAt index xs = read (extractNumber index xs)

main :: IO ()
main = do
    putStrLn "Start..."
    contents <- readFile "input/AoC_2023_d3.txt"
    let input = lines contents
        emptyLine = map (const '.') $ head input
        refined = zip3 (emptyLine:init input) input (tail input ++ [emptyLine])
        activeNumbers = map findActiveNumbers refined
        sumPart1 = (sum . concat) activeNumbers

        -- part 2
        activeNumbersPart2 = map findDuoGears refined
        sumPart2 = (sum . concat) activeNumbersPart2

    putStrLn $ "Result - Part 1: " ++ show sumPart1
    putStrLn $ "Result - Part 2: " ++ show sumPart2


