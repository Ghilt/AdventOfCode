import qualified Data.Map as Map
import Data.Ord

-- Annoying that I do not manage to import that thing, had to write my own
-- https://hackage.haskell.org/package/ilist-0.4.0.1/docs/src/Data.List.Index.html#indexed
-- https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html#g:1
indexed :: [a] -> [(Int, a)]
indexed list = indexed0 0 list
    where indexed0 index (x:xs) = (index, x):(indexed0 (index + 1) xs)
          indexed0 index [] = []

type Argument = Int
type BootSequence = Map.Map Int Instruction

data Command = Nop | Acc | Jmp | EndProgram deriving (Eq, Ord, Show, Read, Bounded, Enum)  

data Instruction = Instruction {command :: Command, arg:: Argument } deriving (Show, Eq, Ord)

endProgram = Instruction {command = EndProgram, arg = 0 }

parseRow :: [String] -> Instruction
parseRow ["jmp", arg] = Instruction Jmp $ read arg
parseRow ["acc", arg] = Instruction Acc $ read arg
parseRow ["nop", arg] = Instruction Nop $ read arg

execute :: BootSequence -> (Bool, Int)
execute bootSequence = executeAt (0, []) 0 
    where executeAt (acc, oldVisited) pc
            | pc `elem` oldVisited = (False, acc)
            | otherwise = 
                let instr = Map.findWithDefault endProgram pc bootSequence 
                    visited = pc:oldVisited  
                in case (command instr) of
                Nop        -> executeAt (acc            , visited) (pc + 1)
                Acc        -> executeAt (acc + arg instr, visited) (pc + 1)
                Jmp        -> executeAt (acc            , visited) (pc + arg instr)
                EndProgram -> (True, acc)

flipBuggedInstruction :: [(Int, Instruction)] -> (Int, Instruction) -> BootSequence
flipBuggedInstruction input (index, instr) = 
    Map.fromList $ case command instr of
        Jmp -> map (flipIt Nop) input
        Nop -> map (flipIt Jmp) input
        _   -> []
    where flipIt to pt@(i, instr) = if index == i 
                                    then (i, Instruction {command = to, arg = arg instr})
                                    else pt

main = do
    contents <- readFile "input/AoC_2020_d8.txt"
    let linesOfFile = lines contents
    let input = indexed $ map (parseRow . words . filter (/='+')) linesOfFile
    let bootSequence = Map.fromList input
    let p1Result = execute bootSequence

    let fixes = filter (not . Map.null) $ map (flipBuggedInstruction input) input 
    let p2Result = filter fst $ map execute fixes

    putStrLn $ "Result - Part 1: " ++ show p1Result
    putStrLn $ "Result - Part 2: " ++ show p2Result
