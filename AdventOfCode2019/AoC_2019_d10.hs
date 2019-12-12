transformToListOfCoordinates :: [String] -> [(Int, Int)]
transformToListOfCoordinates lines =
  map toTuple $ filter filterAsteroids $ concat flat
  where
    iLines = zip [0 ..] $ map (zip [0 ..]) lines
    flat = map integrateIndex iLines
    filterAsteroids (x, y, a) = a == '#'
    toTuple (x, y, a) = (x, y)

integrateIndex :: (Int, [(Int, Char)]) -> [(Int, Int, Char)]
integrateIndex (yc, lst) = map (\x -> (fst x, yc, snd x)) lst

calculateVisibleAsteroids :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
calculateVisibleAsteroids all origin = map toVisionLine centerAsteroids
  where
    centerAsteroids = map (moveBy origin) all
    moveBy (originX, originY) (x, y) = (x - originX, y - originY)

toVisionLine :: (Int, Int) -> (Int, Int)
toVisionLine (0, 0) = (0, 0)
toVisionLine (0, y) = (0, y `div` abs y)
toVisionLine (x, 0) = (x `div` abs x, 0)
toVisionLine (x, y) = (x `div` d, y `div` d)
  where
    d = gcd x y

-- Copied from https://www.rosettacode.org/wiki/Remove_duplicate_elements#Haskell couldn't get a stdlib function to do what I wanted
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (x /=) xs)

main = do
  putStrLn "Start..."
  contents <- readFile "input/AoC_2019_d10.txt"
  let lns = lines contents
  let asteroids = transformToListOfCoordinates lns
  let temp = map (calculateVisibleAsteroids asteroids) asteroids
  let listOfLOS = map length $ map unique temp
  let maxLOS = maximum listOfLOS - 1 -- Minus 1 to compensate for it seeing itself
  putStrLn $ "Part 1: " ++ show maxLOS
