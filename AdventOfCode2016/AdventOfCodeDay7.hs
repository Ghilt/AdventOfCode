--cabal install regex-posix
--import Text.Regex.Posix Could not get backreferences in the regexp to work
import Data.List.Split

splitOnBrackets :: String -> [String] 
splitOnBrackets str = concat ([splitOn "]" x | x <- splitOn "[" str])

checkExistPattern :: String -> Bool
checkExistPattern [] = False 
checkExistPattern (_:_:_:[]) = False
checkExistPattern (a:b:c:d:str) = (a /= b) && (a == d) && (b == c) || (checkExistPattern (b:c:d:str))

checkValid :: [String] -> Bool
checkValid strs = let (good, bad) = isValid strs in not bad && good

isValid :: [String] -> (Bool, Bool)
isValid [] = (False, False)
isValid (x:y:xs) = let (dg, db) = isValid xs in (checkExistPattern x || dg, checkExistPattern y || db)
isValid (x:xs) = (checkExistPattern x, False)

main = do
    putStrLn "Start..."  
    contents <- readFile "input/day7.txt" 
    let linesOfFile = lines contents
    let splitLines = map splitOnBrackets linesOfFile
    let viable = filter checkValid splitLines
    let countViable = length viable
    putStrLn $ "Supports TLS: " ++ show( countViable ) 

-- Regexes for problem two, finds 127+131=258 
-- 
-- Finds all with initial ABA before the BAB
-- (^|^.*\])[^\[\n]*([a-z])((?!\2).)\2.*\[[^\]\n]*\3\2\3.*$
-- Finds all with BAB before ABA
-- ^.*\[[^\]\n]*([a-z])((?!\1).)\1[^\[\n]*\](.*\])?[^\[\n]*\2\1\2.*$