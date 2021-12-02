part1_calculate :: (Int, Int) -> String -> (Int, Int)
part1_calculate (x,y) cmd = case (cmd) of
  ('f':s) -> (x + extract s, y)
  ('u':s) -> (x, y - extract s)
  ('d':s) -> (x, y + extract s)
  where extract abc = read (last $ words abc)

part2_calculate :: (Int, Int, Int) -> String -> (Int, Int, Int)
part2_calculate (x,y,a) cmd = case (cmd) of
  ('f':s) -> (x + extract s, y + a * extract s, a)
  ('u':s) -> (x, y, a - extract s)
  ('d':s) -> (x, y, a + extract s)
  where extract abc = read (last $ words abc)

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2021_d2.txt"
    let linesOfFile = lines contents
    let (x1, y1)     = foldl part1_calculate (0,0) linesOfFile
    let (x2, y2, a2) = foldl part2_calculate (0,0,0) linesOfFile

    putStrLn $ "Result - Part 1: " ++ show (x1 * y1)
    putStrLn $ "Result - Part 2: " ++ show (x2 * y2)