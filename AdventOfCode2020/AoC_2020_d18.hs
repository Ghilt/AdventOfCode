import Data.List
import Data.List.Unique
import Data.Char

import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Expr = Operator Char | Operand Int | Paranthesis Expr Expr Expr | IncompleteParanthesis [Expr] deriving (Show)  

toExpression :: (Expr -> Char -> Expr) -> String -> Expr
toExpression converter tokens = foldl converter (IncompleteParanthesis []) tokens  

convertTokens :: Expr -> Char -> Expr
convertTokens e@(IncompleteParanthesis xs) c
    | isOpenParanthesis xs = IncompleteParanthesis $ (init xs) ++ [convertTokens (last xs) c]
    | length xs == 3 = convertTokens (IncompleteParanthesis [complete e]) c
    | c == ')'  = complete e
    | c == '('  = IncompleteParanthesis (xs ++ [IncompleteParanthesis []])
    | c == '*'  = IncompleteParanthesis (xs ++ [Operator c])
    | c == '+'  = IncompleteParanthesis (xs ++ [Operator c])
    | otherwise = IncompleteParanthesis (xs ++ [Operand (digitToInt c)])

-- Part 1 could easily be converted to fit the part 2 ways of doing things
convertTokensPt2 :: Expr -> Char -> Expr
convertTokensPt2 e@(IncompleteParanthesis xs) c
    | isOpenParanthesis xs = IncompleteParanthesis $ (init xs) ++ [convertTokensPt2 (last xs) c]
    | c == ')'  = completePlusPrecedence e
    | c == '('  = IncompleteParanthesis (xs ++ [IncompleteParanthesis []])
    | c == '*'  = IncompleteParanthesis (xs ++ [Operator c])
    | c == '+'  = IncompleteParanthesis (xs ++ [Operator c])
    | otherwise = IncompleteParanthesis (xs ++ [Operand (digitToInt c)])

complete :: Expr -> Expr
complete (IncompleteParanthesis xs)
    | length xs == 1 = head xs
    | length xs == 3 = Paranthesis (head xs) (xs!!1) (last xs) 

completePlusPrecedence :: Expr -> Expr
completePlusPrecedence (IncompleteParanthesis xs) = head folded
   where folded = foldl foldMultiplyPrecedence [] (foldl foldPlusPrecedence [] xs)

foldPlusPrecedence :: [Expr] -> Expr -> [Expr]
foldPlusPrecedence [] b = [b]
foldPlusPrecedence (a:[]) b = (b:[a])
foldPlusPrecedence (b@(Operator '+'):a:rest) c = ((Paranthesis c b a):rest)
foldPlusPrecedence (b:a:rest) c = (c:b:a:rest)

foldMultiplyPrecedence :: [Expr] -> Expr -> [Expr]
foldMultiplyPrecedence [] b = [b]
foldMultiplyPrecedence (a:[]) b = (b:[a])
foldMultiplyPrecedence (b@(Operator '*'):a:rest) c = ((Paranthesis c b a):rest)
foldMultiplyPrecedence (b:a:rest) c = (c:b:a:rest)

evaluate :: Expr -> Int
evaluate (Operand v) = v
evaluate (Paranthesis a x b) = ((toOperator x) (evaluate a) (evaluate b))

toOperator :: Expr -> (Int -> Int -> Int)
toOperator (Operator '+') = (+)
toOperator (Operator '*') = (*)

isOpenParanthesis :: [Expr] -> Bool
isOpenParanthesis [] = False
isOpenParanthesis xs = isOpen $ last xs
    where
        isOpen :: Expr -> Bool
        isOpen (IncompleteParanthesis _) = True
        isOpen _ = False

main = do
    contents <- readFile "input/AoC_2020_d18.txt"
    let linesOfFile = lines contents
    let noSpace = map (filter (/=' ')) linesOfFile
    let incompleteExpressions = map (toExpression convertTokens) noSpace
    let expressions = map complete incompleteExpressions
    let evaluated = map evaluate expressions
    let summed = foldl1 (+) evaluated

    putStrLn $ "Result - Part 1: " ++ show summed

    -- Part 2

    let incompleteExpressions2 = map (toExpression convertTokensPt2) noSpace
    let expressions2 = map completePlusPrecedence incompleteExpressions2
    let evaluated2 = map evaluate expressions2
    let summed2 = foldl1 (+) evaluated2

    putStrLn $ "Result - Part 1: " ++ show summed2
