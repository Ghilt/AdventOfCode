import Data.List
import Data.List.Unique

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


-- Too slow

{- withoutNeighbour :: [(a,a,a)] -> [a]
withoutNeighbour xs = 
    let midSection = map middleElement xs
        firstElement  (x,_,_) = x
        middleElement (_,x,_) = x
        lastElement   (_,_,x) = x
    in (firstElement $ head xs):midSection ++ [lastElement $ last xs]


withNeighbour2 :: [a] -> [(a,a,a)]
withNeighbour2 xs = zip3 (init xs) (tail $ init xs) (drop 2 xs)


withNeighbour :: Seq.Seq a -> Seq.Seq (a,a,a)
withNeighbour xs = Seq.zip3 withoutLastElement (Seq.drop 1 withoutLastElement) (Seq.drop 2 xs)
    where withoutLastElement = (Seq.deleteAt (length xs - 1) xs)

recurse :: Seq.Seq Int -> Int
recurse xs = foldr checkIt 0 $ withNeighbour xs  
    where checkIt (x,y,z) acc = if z - x < 3 then acc + 1 + (recurse $ Seq.deleteAt (head $ Seq.findIndicesL (find y) xs) xs) else acc
          find x y = x == y  -}

 
{- recurse :: Seq.Seq Int -> Int
recurse xs = Seq.foldrWithIndex checkIt 0 $ xs
    where checkIt i _ acc 
            | i == 0 = acc
            | i + 1 == length xs = acc
            | Seq.index xs (i+1) - Seq.index xs (i-1) < 3 = acc + 1 + (recurse $ Seq.deleteAt i xs)
            | otherwise = acc -}
{- addAcc :: Int -> (Map.Map (Seq.Seq Int) Int, Int) -> (Map.Map (Seq.Seq Int) Int, Int)
addAcc v (memo, acc) = (memo, v + acc) -}

{- recurse :: Set.Set (Seq.Seq Int) -> Seq.Seq Int -> Set.Set (Seq.Seq Int)
recurse memoization xs = Seq.foldlWithIndex checkIt memoization xs
    where 
        checkIt memo i _  
            | i == 0 = memo
            | i + 1 == length xs = memo
            | Set.member (Seq.deleteAt i xs) memo = memo
            | Seq.index xs (i+1) - Seq.index xs (i-1) <= 3 = let newConf = Seq.deleteAt i xs in recurse (Set.insert newConf memo) newConf
            | otherwise = memo -}

recurse :: Set.Set (Seq.Seq Int) -> Seq.Seq Int -> Set.Set (Seq.Seq Int)
recurse memoization xs = Seq.foldlWithIndex checkIt memoization xs
    where 
        checkIt memo i _  
            | i == 0 = memo
            | i + 1 == length xs = memo
            | Set.member (Seq.deleteAt i xs) memo = memo
            | Seq.index xs (i+1) - Seq.index xs (i-1) <= 3 = let newConf = Seq.deleteAt i xs in recurse (Set.insert newConf memo) newConf
            | otherwise = memo


{- recurse2 :: Int -> Int -> Seq.Seq Int -> Int
recurse2 confs sequence = 
        | length sequence <= 2 = confs + 1 
        | length sequence == 3 = if Seq.index sequence (0) - Seq.index sequence (2) <= 3 then confs + 2 else confs + 1
        | length sequence > 3  = recurse2 sequence - -}

main = do
    contents <- readFile "input/AoC_2020_d10.txt"
    let linesOfFile = map read (lines contents) :: [Int]
    let sorted = sort linesOfFile
    let device = [3 + last sorted]
    let differences = zipWith (-) (0:sorted) $ sorted ++ device
    let distribution = count differences

    let sortedWith0andDevice = Seq.fromList (0:sorted ++ device)

    let countConfigs = recurse (Set.fromList [sortedWith0andDevice]) sortedWith0andDevice



    let sortedWith0andDeviceList = tails (0:sorted ++ device)

    putStrLn $ "Result - Part 1: " ++ show distribution
    --putStrLn $ "Result - Part 2: " ++ show countConfigs
    putStrLn $ "Result - Part 2: " ++ show sortedWith0andDeviceList
    putStrLn $ "Result - Part 2: " ++ show (length countConfigs)

