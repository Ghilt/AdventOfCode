import Data.List
import Data.Char
import Control.Applicative
import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import Debug.Trace
import GHC.Parser.CharClass (hexDigit)

map1indexed :: (a -> Int -> b) -> [a] -> [b]
map1indexed f l = zipWith f l [1..]

replace10withX :: String -> String
replace10withX = unpack . replace (pack "10") (pack "A") . pack

data Packet = Value Int | List [Packet] | Incomplete [Packet] deriving (Show)

instance Eq Packet where
  (Value v1) == (Value v2) = v1 == v2
  (List xs) == (List ys) = xs == ys
  a == b = False  

instance Ord Packet where
    compare list@(List _) value@(Value _) = compare list (List [value])
    compare value@(Value _) list@(List _) = compare (List [value]) list
    compare (Value v1) (Value v2)
        | v1 < v2 = LT
        | v1 > v2 = GT
        | otherwise = EQ
    compare (List (x:xs)) (List (y:ys)) = if cc == EQ then compare (List xs) (List ys) else cc
            where cc = compare x y
    compare (List []) (List (y:xs)) = LT
    compare (List (x:xs)) (List []) = GT
    compare (List []) (List []) = EQ

parse :: [String] -> (Packet, Packet)
parse (a:b:[]) = let folder inp = foldr parseFold (Incomplete []) inp in (folder a, folder b)

parseFold :: Char -> Packet -> Packet
parseFold ',' acc = acc
parseFold ']' acc = openDeepest acc
parseFold '[' acc = finishDeepest acc
parseFold  a  acc = addDeepest acc $ hexDigit a

addDeepest :: Packet -> Int -> Packet
addDeepest (Incomplete []) value = Incomplete [Value value]
addDeepest (Incomplete packets@(x:xs)) value
    | allComplete packets = Incomplete ((Value value):packets)
    | otherwise = Incomplete ((addDeepest x value):xs)

openDeepest :: Packet -> Packet
openDeepest (Incomplete []) = Incomplete ([Incomplete []])
openDeepest (Incomplete packets@(x:xs)) 
    | allComplete packets = Incomplete (Incomplete []:packets)
    | otherwise = Incomplete ((openDeepest x):xs)

finishDeepest :: Packet -> Packet
finishDeepest (Incomplete []) = List []
finishDeepest (Incomplete packets@(x:xs)) 
    | allComplete packets = List packets 
    | otherwise = Incomplete ((finishDeepest x):xs)

allComplete :: [Packet] -> Bool
allComplete ((Incomplete _):_) = False  
allComplete _ = True  

cleanUp :: [(Packet, Packet)] -> [(Packet, Packet)] 
cleanUp = map removeOuterIncomplete
    where removeOuterIncomplete ((Incomplete r1), (Incomplete r2)) = (head r1, head r2)

comparePackets :: (Packet, Packet) -> Int
comparePackets (a, b) = toInt $ compare a b
    where
        toInt GT = 0
        toInt LT = 1
        toInt EQ = -1
            

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d13.txt"
    
    let parsed = cleanUp $ map parse $ splitOn [""] $ lines $ replace10withX contents
    let part1Results = map comparePackets parsed
    let part1 = sum $ map1indexed (\v index -> v * index) part1Results

    let createMarker x = List [(List [(Value x)])]
    let m1 = createMarker 2
    let m2 = createMarker 6

    let sorted = sort $ m1:m2:(concatMap (\(x,y) -> [x,y]) parsed) 
    let marker1Pos = elemIndex m1 sorted
    let marker2Pos = elemIndex m2 sorted

    -- https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:mapM_ -- Print each element on its own row
    -- mapM_ print sorted

    -- experiments in adding 1 to two values in maybes and then multiplying them
    let way1 = Just (*) <*> ((+1) <$> marker1Pos) <*> ((+1) <$> marker2Pos)
    let way2 = liftA2 (*) (fmap (+1) marker1Pos) (fmap (+1) marker2Pos)
    let way3 = do x <- marker1Pos
                  y <- marker2Pos
                  return ((x + 1) * (y + 1))

    putStrLn $ "Result - Part 1: " ++ show (part1)
    putStrLn $ "Result - Part 2: " ++ show (way1)
    putStrLn $ "Result - Part 2: " ++ show (way2)
    putStrLn $ "Result - Part 2: " ++ show (way3)
