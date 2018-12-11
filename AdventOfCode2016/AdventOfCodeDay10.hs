import Data.List.Split (splitOn)
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as Map

-- Instruction value target isOutput
data Instruction = Instruction Int Int Bool deriving(Show) 
-- Bot lowTarget highTarget lowValue highValue lowIsOutput highIsOutput
data Bot = Bot Int Int Int Int Bool Bool deriving(Show)

nan = (-1)

stringStartWithB :: String -> Bool
stringStartWithB str = (head str) == 'b'

botConstruct :: String -> Bot
botConstruct str = Bot (read $ sp !! 6) (read $ sp !! 11) nan nan (not $ stringStartWithB (sp !! 5)) (not $ stringStartWithB (sp !! 10))
    where sp = words str

instructionConstruct :: String -> Instruction
instructionConstruct str = Instruction (read $ sp !! 1) (read $ sp !! 5) False
    where sp = words str

botNotEmpty :: Bot -> Bool
botNotEmpty (Bot _ _ v1 v2 _ _) = v1 /= nan || v2 /= nan

botInstructions :: Bot -> (Instruction, Instruction)
botInstructions (Bot lowTarget highTarget v1 v2 lowToOutput highToOutput) = (Instruction v1 lowTarget lowToOutput, Instruction v2 highTarget highToOutput)

botClear :: Bot -> Bot
botClear (Bot lt ht _ _ o1 o2) = (Bot lt ht nan nan o1 o2)

botAddValue :: Int -> Bot -> Bot
botAddValue v (Bot lowTarget highTarget (-1) (-1) o1 o2) = Bot lowTarget highTarget v nan o1 o2
botAddValue v (Bot lowTarget highTarget v1 _ o1 o2) 
    | v1 > v = Bot lowTarget highTarget v v1 o1 o2
    | v1 < v = Bot lowTarget highTarget v1 v o1 o2

instructionTupleToString :: (Instruction, Instruction) -> String
instructionTupleToString (a,b) = show (instructionGetValue a) ++ " : " ++ show (instructionGetValue b)

instructionGetValue :: Instruction -> Int
instructionGetValue (Instruction v1 _ _) = v1

parseInput :: [String] -> ([Instruction], Map.Map Int (Bot, [String]) )
parseInput lns = (map parseInstruction $ filter instructionDescFilter lns, Map.fromList $ map parseBot $ filter stringStartWithB lns)
        where parseBot str = ((read $ words str !! 1), (botConstruct str, []))
              parseInstruction str = instructionConstruct str
              instructionDescFilter str = (head str) == 'v'

executeInstruction :: Map.Map Int (Bot, [String]) -> Instruction -> Map.Map Int (Bot, [String])  
executeInstruction bots (Instruction value target True) = Map.insert target (bb, outInstructionToString:memory) bots
    where (bb, memory) = fromJust $ Map.lookup target bots
          outInstructionToString = (show value ++ " out " ++ (show target))
executeInstruction bots (Instruction value target False)
    | botNotEmpty bb = executeInstruction (executeInstruction mapWithEmptiedBot fstInstr) sndInstr
    | otherwise = Map.insert target (newBot, memory) bots
    where (bb, memory) = fromJust $ Map.lookup target bots
          newBot = botAddValue value bb
          instr@(fstInstr, sndInstr) = botInstructions newBot
          mapWithEmptiedBot = Map.insert target ((botClear bb), (instructionTupleToString instr):memory) bots

filterInteresting :: String -> (Bot, [String]) -> Bool
filterInteresting ('X':t) (_, strs) = (length $ filter (isSuffixOf t) strs) > 0
filterInteresting filterString (_, strs) = elem filterString strs

main = do
    putStrLn "Start..."  
    contents <- readFile "input/day10.txt" 
    let linesInstr = lines contents
        instr = parseInput linesInstr
        process = foldl executeInstruction (snd instr) (fst instr)
        getChip ((_,(Bot _ _ _ _ _ _, (x:xs))):_) = read $ head (words x) :: Int
        multiply l1 l2 l3 = getChip l1 * getChip l2 * getChip l3
        ofInterest1 = Map.filter (filterInteresting "17 : 61") process
        ofInterest2 = Map.filter (filterInteresting "X out 0") process
        ofInterest3 = Map.filter (filterInteresting "X out 1") process
        ofInterest4 = Map.filter (filterInteresting "X out 2") process

    putStrLn $ "Bot warehouse: " ++ show ofInterest1
    putStrLn $ "Bot warehouse: " ++ show (multiply (Map.toList ofInterest2) (Map.toList ofInterest3) (Map.toList ofInterest4)) 
