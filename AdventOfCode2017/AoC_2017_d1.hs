import Data.Char

foldItLooped :: (Int, Int, String) -> Int
foldItLooped (first, accumulator, a:[]) = if digitToInt a == first then accumulator + first else accumulator
foldItLooped (first, accumulator, (a:b:etc)) = let aInt = digitToInt a
                                                   bInt = digitToInt b
                                               in if aInt == bInt then foldItLooped (first, (accumulator + aInt), b:etc) else foldItLooped (first, accumulator, b:etc)

-- // Part 2 \\ --

transform :: [Int] -> [(Int, Int)]
transform s = mapWithIndex (getIntLooped s) s

mapWithIndex :: (Int -> b -> c) -> [b] -> [c]  
mapWithIndex fun list = indexedMapSupport [] list fun list 

indexedMapSupport :: [c] -> [b] -> (Int -> b -> c) -> [b] -> [c]
indexedMapSupport result entireList fun (x:[]) = result ++ (fun (length entireList - 1) x):[]
indexedMapSupport result entireList fun cList@(x:list) = indexedMapSupport (result ++ (fun index x):[]) entireList fun list
                                                            where index = (length entireList - length cList)

getIntLooped :: [Int] -> Int -> Int -> (Int, Int)
getIntLooped list index current = let l = length list
                                      steps = div l 2
                                  in (current, list !! (mod (index + steps) $ l)) 

sumCondition :: Int -> (Int, Int) -> Int
sumCondition accu (frst, snd) = if (frst == snd) then frst + accu else accu                                  

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2017_d1.txt" 
    let sum = foldItLooped (digitToInt $ head contents, 0, contents)
    let specialIndexed = foldl sumCondition 0 $ transform (map digitToInt contents)
    putStrLn $ "Sum for problem 1: " ++ show sum
    putStrLn $ "Sum for problem 2: " ++ show specialIndexed