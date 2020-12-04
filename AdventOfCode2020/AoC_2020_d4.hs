import qualified Data.Text as Text
import Data.List.Split
import Data.Char

groupItems :: [String] -> String -> [String]
groupItems (x:xs) "" = "":x:xs
groupItems (x:xs) newData = (newData ++ " " ++ x):xs

hasRequiredFields :: String -> Bool
hasRequiredFields x = all (flip Text.isInfixOf target) required
    where
      required = map Text.pack ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]
      target = Text.pack x

validateYear :: Int -> Int -> String -> Bool 
validateYear min max field = read field `elem` [min..max]

validateHeight :: String -> Bool 
validateHeight [a, b, c, 'c','m'] = read [a, b, c] `elem` [150..193]
validateHeight [   b, c, 'i','n'] = read [   b, c] `elem` [59..76]
validateHeight _ = False

validateHairColor :: String -> Bool 
validateHairColor ('#':xs) = length xs == 6 && all isAlphaNum xs
validateHairColor _ = False

validateEyeColor :: String -> Bool 
validateEyeColor color = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validateId :: String -> Bool 
validateId id = length id == 9 && all isDigit id

extractField :: String -> [[(String, String)]] -> [String]
extractField target passports = map (\passport -> snd $ head $ filter findTarget passport) passports
  where findTarget x = fst x == target

main = do
    contents <- readFile "input/AoC_2020_d4.txt"
    let linesOfFile = lines contents
    let grouped = foldl groupItems [""] linesOfFile
    let validOnesP1 = filter hasRequiredFields grouped
    let resultP1 = length validOnesP1

    putStrLn $ "Result - Part 1: " ++ show resultP1

    -- part 2
    let makeTuple [a, b] = (a, b)
    let info = map (map makeTuple) $ map (map $ splitOn ":") $ map words validOnesP1
    let compress a b = map a $ extractField b info

    let validations = 
          [ compress (validateYear 1920 2002) "byr"
          , compress (validateYear 2010 2020) "iyr"
          , compress (validateYear 2020 2030) "eyr"
          , compress validateHeight           "hgt"
          , compress validateHairColor        "hcl" 
          , compress validateEyeColor         "ecl"
          , compress validateId               "pid" ]

    let combined = foldl (zipWith (&&)) (repeat True) validations
    let result = length $ filter (not . not) combined

    putStrLn $ "Result - Part 2: " ++ show result