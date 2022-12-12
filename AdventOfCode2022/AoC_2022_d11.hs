import Data.List
import Data.Char
import Data.List.Split (splitOn)
import Data.Text (pack, count)
import Debug.Trace
-- import Control.Lens --https://hackage.haskell.org/package/lens wanted to install this but couldn't

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

type Id = Integer
type Items = [Integer]
type Operation = Integer -> Integer -> Integer
type Operand = Integer -> Integer -- Sometimes the second operand depends on the previous worry value
type Test = Integer -> Integer

instance Show Operation where
    show a = show (deduce $ a 0 1)
        where
        deduce 0 = '*'
        deduce 1 = '+' 
        deduce a = '?' 

data Monkey = Monkey Id Items Operation Operand Test
instance Show Monkey where
    show (Monkey id items op operand test) = "{" ++ show id ++ " " ++ show items ++ " " ++ show op ++ "}"
instance Eq Monkey where
    (Monkey id1 _ _ _ _) == (Monkey id2 _ _ _ _) = id1 == id2

isId :: Integer -> Monkey -> Bool
isId id (Monkey mId _ _ _ _) = id == mId

parse :: [String] -> Monkey
parse (idStr:itemStr:opStr:testStr:tStr:fStr:[]) = Monkey id items op operand test
    where
        id = read $ init $ last $ words idStr :: Integer
        items = map read $ splitOn "," $ filter (not . isSpace) $ last $ splitOn ":" itemStr :: [Integer]
        operationLine = words opStr
        op
         | operationLine !! 4 == "+" = (+) -- (it would be cool if theres a like a package for 'read' for operators with :: (int -> int -> int). Maybe there is)
         | operationLine !! 4 == "*" = (*)
        operand = if last operationLine == "old" then (\x -> x) else (\x -> lastInt opStr)
        test = (\x -> lastInt (if 0 == x `rem` (lastInt testStr) then tStr else fStr))
        
lastInt :: String -> Integer
lastInt = read . last . words

performTurn :: Monkey -> [Monkey] -> [Monkey]
performTurn m@(Monkey id items op operand test) monkeys = throwAllOfMyOwnItemsAway $ foldl evaluator monkeys items
    where 
        evaluator monkeys item = let (newWorry, targetMonkey) = inspectItem item m in throwToMonkey newWorry targetMonkey monkeys
        throwAllOfMyOwnItemsAway = map (\mnky -> if mnky == m then Monkey id [] op operand test else mnky)
        
inspectItem :: Integer -> Monkey -> (Integer, Integer)     
inspectItem item (Monkey id items op operand test) = (inspected, test inspected)
    where inspected = div (op item $ operand item) 3

throwToMonkey :: Integer -> Integer -> [Monkey] -> [Monkey]
throwToMonkey item to monkeys = map (\m -> if isId to m then monkeyCatch item m else m) monkeys

monkeyCatch :: Integer -> Monkey -> Monkey
monkeyCatch item (Monkey id items op operand test) = (Monkey id (item:items) op operand test)

performRound :: [(Integer, Monkey)] -> Int -> [(Integer, Monkey)] 
performRound xs size = foldl countInspections xs [x | x <- [0..(size - 1)]]
    where
        countInspections acc mIndex = 
            let (count, m@(Monkey _ items _ _ _)) = acc !! mIndex 
            in zipWith increaseCount acc $ mapInd (\x index -> (index, (toInteger $ length items), x)) $ performTurn m $ map snd acc
                where
                    increaseCount (count, _) (index, count2, m) = (if index == mIndex then count + count2 else count, m)

main = do  
    putStrLn "Start..."   
    contents <- readFile "input/AoC_2022_d11.txt"

    let rawMonkeys = map parse $ splitOn [""] $ lines contents
    let monkeys = map (\x -> (toInteger 0, x)) rawMonkeys

    let rounds = foldl performRound monkeys [length monkeys | x <- [0..(20 - 1)]]

    putStrLn $ "Result - Part 1: " ++ show (rounds)
    -- Running it for part 2 gives out of memory error
