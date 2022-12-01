import Data.List.Split
import Data.List
import Debug.Trace -- I will do some tracing just so i don't forget that it exists

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d1.txt"
    let caloriesPerElf = splitOn [""] $ lines contents
    let caloriesAsInt = map (\elfBag -> {- trace ("Elements in each bag = " ++ show (length elfBag)) $ -} map read elfBag :: [Int]) caloriesPerElf
    let maxCal = maximum $ map sum caloriesAsInt
    let max3Cal = sum $ take 3 $ reverse $ sort $ map sum caloriesAsInt

    -- Transformation from 'normal programmer brain'-style to point free style
    let max3Cal_PointFreeStyle0 = sum $ take 3 $ reverse $ sort $ map sum caloriesAsInt
    let max3Cal_PointFreeStyle1 = sum ( take 3 ( reverse ( sort (map sum caloriesAsInt))))
    let max3Cal_PointFreeStyle2 x = sum ( take 3 ( reverse ( sort (map sum x))))
    let max3Cal_PointFreeStyle3 = sum . (take 3) . reverse . sort . (map sum)
    -- That's just amazing that I managed to rewrite something in point free style in just 15 minutes
    -- So the pointfree solution composes a function with {in my brain} has sum as the inner layer and outside the sum there is a little gnome only taking 3 elements of the inputted list before passing it on
    -- and outside the take 3 gnome there sits a reverse gnome, he is just reversing the list. 
    -- and outside of him sits a sorting gnome
    -- and outside of him sits a complicated gnome, he doesn't want just any list, he craves a list of lists which he he then hands over as just a regular list to the sorting gnome


    putStrLn $ "Result - Part 1: " ++ show maxCal
    putStrLn $ "Result - Part 2: " ++ show max3Cal
    putStrLn $ "Result - Part 2: " ++ show (max3Cal_PointFreeStyle3 caloriesAsInt)