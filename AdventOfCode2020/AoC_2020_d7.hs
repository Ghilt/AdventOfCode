import qualified Data.Map as Map
import Data.Ord

--https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
--https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html

type Color = String
type Style = String

data Bag = Bag Style Color deriving (Show, Eq, Ord)

goldBag = Bag "shiny" "gold"
isTrue b = b

parseRow :: [String] -> (Bag, [(Int, Bag)])
parseRow (style:color:["contain", "no", "other", "bags."]) = (Bag style color, [])
parseRow list = (Bag (head list) (list !! 1), contents (drop 4 list))
    where contents (amount:style:color:_:xs) = (read amount, Bag style color):(contents xs)
          contents _ = []

containsGold :: Map.Map Bag [(Int, Bag)] -> [(Int, Bag)] -> Bool
containsGold rules [] = False 
containsGold rules values = if containsGoldBag values 
                            then True
                            else any isTrue $ map (containsGold rules . openBag . snd) values
        where openBag bag = Map.findWithDefault [] bag rules       
                     

containsGoldBag :: [(Int, Bag)] -> Bool
containsGoldBag list = goldBag `elem` map snd list

countContainedBags :: Map.Map Bag [(Int, Bag)] -> (Int, Bag) -> Int
countContainedBags rules (amount, bag) = bagCountThisStep * amount + sum (map (countContainedBags rules) bagsToOpen)
    where 
       bagsInCurrentBag = Map.findWithDefault [] bag rules
       bagCountThisStep = sum $ map fst bagsInCurrentBag
       bagsToOpen = map (\(c, b) -> (c * amount, b)) bagsInCurrentBag


main = do
    contents <- readFile "input/AoC_2020_d7.txt"
    let linesOfFile = lines contents
    let rules = Map.fromList $ map (parseRow . words) linesOfFile

    let goldWithin = Map.map (containsGold rules) rules    

    let bagCount = Map.size $ Map.filter isTrue goldWithin

    let containedBags = countContainedBags rules (1, goldBag)

    putStrLn $ "Result - Part 1: " ++ show bagCount
    putStrLn $ "Result - Part 2: " ++ show containedBags
