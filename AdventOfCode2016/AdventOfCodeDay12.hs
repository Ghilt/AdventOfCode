import Data.List
import Data.Char

data Registry = Registry Int | RInt Int deriving (Show)
data Instruction = Cpy Registry Registry | Inc Registry | Dec Registry | Jnz Registry Registry deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction ('c':str) = let p = words str in Cpy (parseRegistry $ p !! 1) (parseRegistry $ p !! 2)  
parseInstruction ('i':str) = let p = words str in Inc (parseRegistry $ p !! 1)
parseInstruction ('d':str) = let p = words str in Dec (parseRegistry $ p !! 1)
parseInstruction ('j':str) = let p = words str in Jnz (parseRegistry $ p !! 1) (parseRegistry $ p !! 2)  

parseRegistry :: String -> Registry
parseRegistry str@(x:_)
    | isLetter x = Registry $ (ord x - ord 'a')
    | otherwise = RInt (read str :: Int)

doCommand :: Instruction -> [Int] -> (Int, [Int])
doCommand (Cpy (Registry src) (Registry target)) rs = (1, replaceElement target (rs !! src) rs)
doCommand (Cpy (RInt src) (Registry target)) rs = (1, replaceElement target src rs)
doCommand (Dec (Registry target)) rs = (1, replaceElement target (rs !! target - 1) rs)
doCommand (Inc (Registry target)) rs = (1, replaceElement target (rs !! target + 1) rs)
doCommand (Jnz cond (RInt target)) rs = if (notTrue cond) then (1, rs) else (target, rs)
    where notTrue (Registry reg) = (rs !! reg) == 0
          notTrue (RInt val) = (val == 0)

replaceElement :: Int -> a -> [a] -> [a]
replaceElement i r list = x ++ r:zs
    where (x, _:zs) = splitAt i list 

executeInstructions :: [Instruction] -> (Int, [Int]) -> (Int, [Int])
executeInstructions instrs (i, regs)
    | i >= length instrs = (i, regs)
    | otherwise = 
        let performCmd cmd = toAbsoluteIndex $ doCommand cmd regs
            toAbsoluteIndex (relative, rs) = (i+relative, rs)   
        in executeInstructions instrs (performCmd (instrs !! i)) 

main = do
    putStrLn "Start..."  
    contents <- readFile "input/day12.txt" 
    let linesInstr = lines contents
        instr = map parseInstruction linesInstr
        endState = executeInstructions instr (0, [0,0,0,0])
        endState2 = executeInstructions instr (0, [0,0,1,0])
    putStrLn $ "Asembunny code: " ++ show endState
    putStrLn $ "Asembunny code: " ++ show endState2
