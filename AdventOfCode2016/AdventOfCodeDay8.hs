import Data.List.Split (splitOn)
import Data.List
import Data.Foldable (toList)
import Data.Sequence (update, fromList)

screenWidth = 50
screenHeight = 6

screenAccumulator ::[String] -> (String,(Int, Int)) -> [String] 
screenAccumulator space ("rect", (a,b)) = zipWith (zipWith orMatrix) (newRect a b) space
    where orMatrix x y = if x == '#' || y == '#' then '#' else '.' 
screenAccumulator space ("row", (a,b)) = let rowOverflow = splitAt (screenWidth - b) (space !! a) 
                                         in toList $ update a (snd rowOverflow ++ (fst rowOverflow)) $ fromList space
screenAccumulator space ("column", (a,b)) = let transformDim = screenWidth - screenHeight + b -- ugly hack
                                            in transpose (screenAccumulator (transpose space) ("row", (a, transformDim))) -- Probably ineffiecient

reformat :: String -> (String,(Int, Int)) -- Should learn how to make my own typeClasses
reformat = splitFurther . splitOn " "
    where splitFurther ["rect", v] = ("rect", let splitRect = splitOn "x" in (toInt $ head $ splitRect v,toInt $ last $ splitRect v))
          splitFurther ["rotate", dim, v, _, x] = (dim,(getTargetIndex v, toInt x))   
          getTargetIndex = toInt . last . splitOn "="
          toInt x = read x :: Int

startScreen = let mkWidth = take screenWidth (repeat '.') 
              in replicate screenHeight mkWidth -- Using different functions for the sake of using different functions

newRect :: Int -> Int -> [String]
newRect width height = let firstline = replicate width '#' ++ replicate (screenWidth - width) '.' 
                           emptyLine = replicate screenWidth '.'
                       in replicate height firstline ++ replicate (screenHeight - height) emptyLine 

main = do
    putStrLn "Start..."  
    contents <- readFile "input/day8.txt" 
    let linesOfFile = lines contents
    let splitLines = map reformat linesOfFile
    let finalRender = foldl screenAccumulator startScreen splitLines
    let countOnPx = length $ filter (=='#') (concat finalRender)
    putStrLn $ "Final render pixels on: " ++ show( countOnPx) 
