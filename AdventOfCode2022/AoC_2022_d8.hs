import Data.List
import Data.Char 
import Debug.Trace
import Data.Set (fromList)

mapToCoordinate :: (a -> b) -> Int -> [a] -> [(Int, Int, b)]
mapToCoordinate f y l = zipWith zipToIndexTuple l [0..]
    where zipToIndexTuple a x = (x, y, f a)

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

filterVisible :: [(Int, Int, Int)] -> [(Int, Int, Int)] 
filterVisible = combine . foldl look ([], []) 
    where combine (as, bs) = as ++ bs     

neg1OrMax :: [Int] -> Int
neg1OrMax [] = -1
neg1OrMax xs = maximum xs

dMap :: ([a] -> a -> b) -> [[a]] -> [[b]]
dMap f xs = map (\list -> map (f list) list) xs

look :: ([(Int, Int, Int)], [(Int, Int, Int)]) -> (Int, Int, Int) -> ([(Int, Int, Int)], [(Int, Int, Int)])
look (fromFront, fromBack) c@(_, _, z)
    | z > maxZ = (c:fromFront, [c])
    | z < maxZ = (fromFront, (c:fromBackObscuredByNew))
    | z == maxZ = (fromFront, [c])
    where maxZ = neg1OrMax $ map (\(_,_,zz) -> zz) fromFront -- same as maximum fromBack, they both contain the highest tree
          fromBackObscuredByNew = filter (\(_,_, zz) -> zz > z ) fromBack

countSmaller :: Int -> [Int] -> Int
countSmaller height = snd . foldl countDesc (0, 0)
    where countDesc acc@(lastAdded, count) tree = 
            if lastAdded >= height 
            then {- (trace (" no  " ++ show (height, acc, tree) )) $ -} acc 
            else {- (trace (" yes " ++ show (height, acc, tree) )) $ -} (tree, count + 1)

scoreTreeHouse :: Int -> [Int] -> Int
scoreTreeHouse position trees = (countSmaller $ last $ take pos trees) . (drop pos) $ trees
    where pos = {-(trace ("t___ " ++ show (position) )) $-} position + 1

oTree :: [(Int, Int, Int)] -> [Int]
oTree = map (\(x,y,z) -> z)

calcScenicScores :: [(Int, Int, Int)] -> [(Int, Int, Int)] -- Slow! But it runs in the minutes, so thats ok!
calcScenicScores = foldl calcScenic []
    where calcScenic acc entry@(x,y,z) 
            | 0 == (length $ filter (\(xx,yy,zz) -> x == xx && y == yy) acc) = (entry:acc)   
            | otherwise = foldl (multiplyInNewView entry) [] acc  
          multiplyInNewView (x, y, viewScore) acc current@(xCurrentScore, yCurrentScore, currentViewScore)
            | x == xCurrentScore && y == yCurrentScore = ((x, y, viewScore * currentViewScore):acc) 
            | otherwise = (current:acc)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d8.txt"

    let xyz = mapInd (\z y -> mapToCoordinate digitToInt y z) $ lines contents
    let yxz = transpose xyz
    let rXyz = map reverse xyz
    let rYxz = map reverse yxz
    let size = length xyz - 1

    let together = fromList $ concat $ (map filterVisible xyz) ++ (map filterVisible yxz)

    let front  = dMap (\trees (x,y,_) -> (x, y, scoreTreeHouse x          $ oTree trees)) xyz
    let back   = dMap (\trees (x,y,_) -> (x, y, scoreTreeHouse (size - x) $ oTree trees)) rXyz
    let top    = dMap (\trees (x,y,_) -> (x, y, scoreTreeHouse y          $ oTree trees)) yxz
    let bottom = dMap (\trees (x,y,_) -> (x, y, scoreTreeHouse (size - y) $ oTree trees)) rYxz

    let scores = calcScenicScores $ (concatMap id front) ++ (concatMap id back) ++ (concatMap id top) ++ (concatMap id bottom)
    let highestScore = maximum . map (\(x,y,z) -> z) . calcScenicScores

    putStrLn $ "Result - Part 1: " ++ show (length together)
    putStrLn $ "Result - Part 2: " ++ show (highestScore scores) -- 385112


