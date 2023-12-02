{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.List.Split as Split
import Data.Char

-- Later expieriment with pretty print, could not get to work
-- import Text.Pretty.Simple (pPrint)
-- not even with runhaskell -package pretty-simple AoC_2023_d2.hs

import Debug.Trace -- I will do some tracing just so i don't forget that it exists

data Color = Red | Green | Blue
  deriving (Enum, Show, Eq)

pairConsecutive :: [a] -> [(a, a)]
pairConsecutive xs = map (\l -> (head l, last l)) $ Split.chunksOf 2 xs

convert :: (String, String) -> (Color, Int)
convert (a,'r':xs) = (Red, read a)
convert (a,'b':xs) = (Blue, read a)
convert (a,'g':xs) = (Green, read a)

splitOnPredicate :: ([Char] -> Bool) -> [String] -> [[String]]
splitOnPredicate predicate xs =
  case break predicate xs of
    (group, rest) ->
      if null rest
        then [group]
        else (group ++ [head rest]): splitOnPredicate predicate (tail rest)

breakList :: (a -> Bool) -> [a] -> [[a]]
breakList breakElement xs = case break breakElement xs of
  (prefix, suffix) -> [prefix ++ suffix, tail suffix]

compareToRequirement :: [(Color, Int)] -> (Int, [(Color, Int)]) -> Bool
compareToRequirement requirements (_, (color, amount):xs) = all (\(color, amount) -> any (\(c,maxAmount) -> color == c && maxAmount >= amount) requirements) xs


parse :: String -> (Int, [[(Color, Int)]])
parse desc = (read $ init $ head asWords, map (map convert . pairConsecutive) sets)
  where asWords = drop 1 $ words desc
        sets = splitOnPredicate (elem ';') $ drop 1 asWords

powerOfEach :: [(Color, Int)] -> Int
powerOfEach xs = maxforColor Red xs * maxforColor Green xs * maxforColor Blue xs

maxforColor :: Color -> [(Color, Int)] -> Int
maxforColor color xs = maximum $ map snd $ filter (\i -> color == fst i ) xs

main :: IO ()
main = do
    putStrLn "Start..."
    contents <- readFile "input/AoC_2023_d2.txt"
    let input = lines contents
        requirement = [(Red, 12), (Green, 13), (Blue, 14)]
        parsed = map parse input
        flattened = map (\(id, picks) -> (id, concat picks)) parsed
        successfulGames = sum $ map fst $ filter (compareToRequirement requirement) flattened
        part2 = sum $ map (\(_, list) -> powerOfEach list) flattened

    putStrLn $ "Result - Part 1: " ++ show successfulGames

    putStrLn $ "Result - Part 1: " ++ show part2

