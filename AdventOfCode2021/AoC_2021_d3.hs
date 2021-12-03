import Data.List
import Numeric
import Data.Function

fDiv :: Int -> Int -> Float
fDiv = (/) `on` fromIntegral

countChar :: Char -> String -> Int
countChar c str = length $ filter (== c) str

binToDec :: String -> Int
binToDec = fst . head . readBin

oneOrZero :: (a -> Bool) -> a -> Char
oneOrZero condition input = if condition input then '1' else '0'

hasCharAtPos :: Int -> Char -> String -> Bool
hasCharAtPos pos c xs = xs!!pos == c

recurse :: (Int -> Int -> Bool) -> Char -> Char -> Int -> [String] -> String
recurse decide c o index (last:[]) = last
recurse decide c o index list = getWinners $ findCharOfPosition decide c o index list
  where getWinners w = recurse decide c o (index + 1) (filter (hasCharAtPos index w) list)

findCharOfPosition :: (Int -> Int -> Bool) -> Char -> Char -> Int -> [String] -> Char
findCharOfPosition decide c o index list = if (decide charCount halfLengthRoundedUp) then c else o
  where len = length list
        halfLengthRoundedUp = len `div` 2 + len `rem` 2
        charCount = countChar c $ (transpose list)!!index

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2021_d3.txt"
    let rowsOfFile = lines contents
    let columnsOfFile = transpose rowsOfFile
    let len = length $ head columnsOfFile
    -- say what, I managed to use a '.' without much trouble. I had a function which missed an argument and I imagined function composition flowing backwards and it just compiled like magic
    let gammaRate = map ((oneOrZero (> len `div` 2)) . (countChar '1' )) columnsOfFile
    let epsilonRate = map (oneOrZero (== '0')) gammaRate
    let part1 = (binToDec gammaRate) * (binToDec epsilonRate)

    let oxygenRating = recurse (>=) '1' '0' 0 rowsOfFile  
    let co2ScrubberRating = recurse (<) '1' '0' 0 rowsOfFile  
    let part2 = (binToDec oxygenRating) * (binToDec co2ScrubberRating)

    putStrLn $ "Result - Part 1: " ++ show (part1)
    putStrLn $ "Result - Part 2: " ++ show (part2)
