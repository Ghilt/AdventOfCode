import Data.List.Split

test1 :: String -> Int
test1 a = 123 

test2 :: Int -> Int -> String
test2 a b = "fsy5"

xor :: Bool -> Bool -> Bool
xor a b 
    | a && b = False
    | not a && not b = False
    | otherwise = True
 
isValidPassword :: [String] -> Bool
isValidPassword (min:max:letter:password:[]) = countLetter >= read min && countLetter <= read max
    where 
      countLetter = length $ filter (== head letter) password 

isTobogganPassword :: [String] -> Bool
isTobogganPassword (min:max:target:password:[]) = xor (letter == password !! firstPos) (letter == password !! secondPos)
    where 
      countLetter = length $ filter (== letter) password
      letter = head target 
      firstPos = read min - 1 
      secondPos = read max - 1 

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2020_d2.txt"
    let linesOfFile = map (wordsBy (\x-> elem x [' ','-',':'])) $ lines contents
    let resultsD1 = length $ filter (\r -> r) $ map isValidPassword linesOfFile
    let resultsD2 = length $ filter (\r -> r) $ map isTobogganPassword linesOfFile
    let resultsD3 = test1 . test2 $ 4563

    putStrLn $ "Result - Part 1: " ++ show resultsD1
    putStrLn $ "Result - Part 2: " ++ show resultsD3

