import Data.Char

futRec = 10

data Recipes = Recipes Int Int Int [Int] deriving(Show) 

-- Stolen, don't understand pointfree notation right now
digits :: Integer -> [Int]
digits = map (read . (:[])) . show

iterateRecipes :: Recipes -> [Int]
iterateRecipes r@(Recipes target e1 e2 list)
    | length list >= target + futRec = snd $ splitAt target list
    | otherwise = iterateRecipes $ addRecipes r 

addRecipes :: Recipes -> Recipes
addRecipes (Recipes t e1 e2 list) = 
    let (e1c, e2c) = (list !! e1, list !! e2)
        newVal = e1c + e2c
        newList = list ++ (digits $ toInteger newVal) -- toInteger is so weird to have here also
        e1n = calcNewPos e1 e1c newList
        e2n = calcNewPos e2 e2c newList
    in Recipes t e1n e2n newList

calcNewPos :: Int -> Int -> [Int] -> Int    
calcNewPos i i2 list = (i + i2 + 1) `mod` (length list)

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d14.txt"
    let input = read contents :: Int
    let startingPoint = Recipes input 0 1 [3,7]
    putStrLn $ "The next recipes are: " ++ show (iterateRecipes startingPoint) 
    -- [1,4,1,3,1,3,1,3,3,9] Took an hour to run! I know ++'ing lists is bad compared to :'ing lists.  
    -- Another possible horrible thing could be that I convert ints to string to extract the digits
    -- Would be interesting to do these optimizations to see if it's correct that those are the culprits