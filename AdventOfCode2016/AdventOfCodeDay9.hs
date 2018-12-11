import Data.List.Split (splitOn)
import Data.List

data RepSeq = RepSeq Int Int String | UnfinishedCommand String deriving (Show)
data RepSeqTree = RepSeqTree RepSeq [RepSeqTree] deriving (Show)

parseInput :: String -> [RepSeq]
parseInput = checkFinalElement . foldl traverser []
    where checkFinalElement (UnfinishedCommand str:instrs) = (RepSeq (length str) 1 str):instrs
          checkFinalElement list = list

traverser :: [RepSeq] -> Char -> [RepSeq] 
traverser [] c = UnfinishedCommand (c:[]) :[]
traverser (UnfinishedCommand []:instrs) c = (UnfinishedCommand $ c:[]):instrs
traverser (UnfinishedCommand ('(':xs):instrs) ')' = (createCommand xs):instrs
traverser (UnfinishedCommand str@('(':xs):instrs) c = (UnfinishedCommand $ str ++ c:[]):instrs
traverser (UnfinishedCommand str:instrs) '(' = UnfinishedCommand "(":(RepSeq (length str) 1 str):instrs
traverser (UnfinishedCommand str:instrs) c = (UnfinishedCommand $ str ++ c:[]):instrs
traverser (rs@(RepSeq len rep con):instrs) c 
                        | length con /= len = (RepSeq len rep (con ++ c:[])):instrs
                        | otherwise = (UnfinishedCommand $ c:[]):rs:instrs

decompress :: [RepSeq] -> String
decompress list = concat $ map convert list
            where convert (RepSeq len reps content) = concat $ replicate reps content
            
createCommand :: String -> RepSeq
createCommand src = RepSeq len reps ""
                    where tt = splitOn "x" src 
                          len = (read (tt !! 0) :: Int) 
                          reps =(read (tt !! 1) :: Int)

createContentTree :: String -> RepSeqTree
createContentTree list = RepSeqTree rootNode $ map extract $ parseInput list
        where extract node@(RepSeq len rep content) 
                    |  '(' `elem` content = RepSeqTree node $ map extract (parseInput content)
                    | otherwise = RepSeqTree node []
              rootNode = RepSeq 0 1 ""

calcuateLengthOfContentTree :: Int -> RepSeqTree -> Int
calcuateLengthOfContentTree mpx (RepSeqTree (RepSeq len reps _) []) = mpx * len * reps
calcuateLengthOfContentTree mpx (RepSeqTree (RepSeq _ reps _) list) = foldl (+) 0 $ map (calcuateLengthOfContentTree $ mpx * reps) list  

main = do
    putStrLn "Start..."  
    contents <- readFile "input/day9.txt" 
    let replicateSequences = parseInput contents
        decompressed = decompress replicateSequences
        fullyDecompressedLength = calcuateLengthOfContentTree 1 $ createContentTree contents
    putStrLn $ "Single pass decompressed length: " ++ show(length decompressed) 
    putStrLn $ "Fully decompressed length: " ++ show(fullyDecompressedLength) 

--fullyDecompress :: String -> String -- Stack overflow
--fullyDecompress inpt
--                | '(' `elem` inpt = fullyDecompress $ decompress $ parseInput inpt 
--               | otherwise = inpt