import Data.Char
import qualified Data.Sequence as Seq

futRec = 10

-- Test inputs for part 2
--part2Input =  Seq.fromList [1,6,5,0,4,5]
--part2Input2 = Seq.fromList [9,8,5,1,5]
--part2Input3 = Seq.fromList [5,4,2,1,0]
--part2Input4 = Seq.fromList[0,1,5,2,9]
--part2Input5 = Seq.fromList[4,1,4,9,5]

data Recipes = Recipes Int Int Int (Seq.Seq Int) deriving(Show) 

-- Stolen, don't understand pointfree notation right now
digits :: Int -> [Int]
digits = map (read . (:[])) . show

iterateRecipes :: Recipes -> (Seq.Seq Int)
iterateRecipes r@(Recipes target e1 e2 seq)
    | Seq.length seq >= target + futRec = Seq.take 10 seq
    | otherwise = iterateRecipes $ addRecipes r 


-- Reversed everything to build the list in reverse but still gets: getMBlocks: VirtualAlloc MEM_COMMIT failed: The paging file is too small for this operation to complete.
-- Also wrote out some things in an attempt to find the space/mem leak
-- So I think the solution for part 2 is correct but that something eats memory
iterateRecipesPart2 :: Recipes -> Int
iterateRecipesPart2 r@(Recipes _ e1 e2 seq)
    -- | Seq.take 5 seq == part2Input2 = Seq.length seq - 5
    -- | Seq.length seq > 5 && bac 1:bac 2:bac 3:bac 4: bac 5:bac 6:[] == part2Input = Seq.length seq - 6
    -- | Seq.length seq > 1000000000 = Seq.length seq - 6 -- Test: i know the real list do not grow beyond 1000 000 000
    | Seq.length seq > 5 && (bac 0 == 1) && (bac 1 == 6) && (bac 2 == 5) && (bac 3 == 0) && (bac 4 == 4) && (bac 4 == 5) = Seq.length seq - 6
    -- | Seq.length seq > 5 && (bac 0 == 4) && (bac 1 == 1) && (bac 2 == 4) && (bac 3 == 9) && (bac 4 == 5) = Seq.length seq - 5
    -- | Seq.take 5 (Seq.reverse seq) == part2Input5 = Seq.length seq - 5
    -- | Seq.length seq >= 26 = seq
    | otherwise = iterateRecipesPart2 $ addRecipes r
        where bac i = Seq.index seq i
             

addRecipes :: Recipes -> Recipes
addRecipes (Recipes t e1 e2 seq) = 
    let (e1c, e2c) = (Seq.index seq e1, Seq.index seq e2)
        newVal = e1c + e2c
        newPart = reverse (digits newVal)
        newSeq = (Seq.fromList newPart) Seq.>< seq
        e1n = calcNewPos (length newPart + e1) e1c newSeq
        e2n = calcNewPos (length newPart + e2) e2c newSeq
    in Recipes t e1n e2n newSeq

calcNewPos :: Int -> Int -> (Seq.Seq Int) -> Int    
calcNewPos i i2 seq = (i - i2 - 1) `mod` (Seq.length seq)


main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2018_d14.txt"
    let input = read contents :: Int
    let startingPoint = Recipes input 1 0 (Seq.fromList [7,3])
    putStrLn $ "Int maxbound: " ++ show (maxBound :: Int)
    putStrLn $ "The next recipes are: " ++ show startingPoint
    putStrLn $ "The part1 recipes are: " ++ show (iterateRecipes startingPoint) 
    putStrLn $ "The part2 answer: " ++ show (iterateRecipesPart2 startingPoint) 
