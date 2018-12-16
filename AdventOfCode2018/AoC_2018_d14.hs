import Data.Char
import qualified Data.Sequence as Seq

futRec = 10
part2Input =  Seq.fromList [1,6,5,0,4,5]
--part2Input2 = [9,8,5,1,5]
--part2Input3 = [5,4,2,1,0]
--part2Input4 = [0,1,5,2,9]
--part2Input5 = [4,1,4,9,5]

data Recipes = Recipes Int Int Int (Seq.Seq Int) deriving(Show) 

-- Stolen, don't understand pointfree notation right now
digits :: Int -> [Int]
digits = map (read . (:[])) . show

iterateRecipes :: Recipes -> (Seq.Seq Int)
iterateRecipes r@(Recipes target e1 e2 seq)
    | Seq.length seq >= target + futRec = snd $ Seq.splitAt target seq
    | otherwise = iterateRecipes $ addRecipes r 

iterateRecipesPart2 :: Recipes -> Int
iterateRecipesPart2 r@(Recipes _ e1 e2 seq)
    -- | Seq.take 6 (Seq.reverse seq) == part2Input = Seq.length seq - 5
    -- | Seq.length seq > 5 && bac 1:bac 2:bac 3:bac 4: bac 5:bac 6:[] == part2Input = Seq.length seq - 6
    | len > 5 && (bac 1 == 1) && (bac 2 == 6) && (bac 3 == 5) && (bac 4 == 0) && (bac 5 == 4) && (bac 6 == 5) = Seq.length seq - 6
    -- | Seq.take 5 (Seq.reverse seq) == part2Input5 = Seq.length seq - 5
    | otherwise = iterateRecipesPart2 $ addRecipes r
        where len = Seq.length seq 
              bac i = Seq.index seq (len - i)

addRecipes :: Recipes -> Recipes
addRecipes (Recipes t e1 e2 seq) = 
    let (e1c, e2c) = (Seq.index seq e1, Seq.index seq e2)
        newVal = e1c + e2c
        newSeq = seq Seq.>< Seq.fromList (digits newVal)
        e1n = calcNewPos e1 e1c newSeq
        e2n = calcNewPos e2 e2c newSeq
    in Recipes t e1n e2n newSeq

calcNewPos :: Int -> Int -> (Seq.Seq Int) -> Int    
calcNewPos i i2 seq = (i + i2 + 1) `mod` (Seq.length seq)

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d14.txt"
    let input = read contents :: Int
    let startingPoint = Recipes input 0 1 (Seq.fromList [3,7])
    putStrLn $ "The next recipes are: " ++ show startingPoint
    putStrLn $ "The part1 recipes are: " ++ show (iterateRecipes startingPoint) 
    putStrLn $ "The part2 answer: " ++ show (iterateRecipesPart2 startingPoint) 
