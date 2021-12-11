import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace
{-
 **** Important --- import Debug.Trace is awesome, it allows you to print in the middle of your pure code, truly revolutionary. Have I really missed that in all my years doing haskell AoC?
Or is this simply rediscovered knowledge
-}

-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

type Circuits = Map.Map Int Char

placeWire :: Char -> Int -> Circuits -> Circuits
placeWire c pos circuits = Map.insert pos c circuits

emptyCircuits :: Circuits
emptyCircuits = Map.empty

canAccept :: Int -> Circuits -> Bool
canAccept pos circuits = Map.notMember pos circuits

startingRestraints :: [(Int, Int)]
startingRestraints = [(2, 1), (3, 7), (4, 4), (7, 8)]

digitSegments :: Map.Map Int [Int]
digitSegments = Map.fromList[(0, [0,1,2,3,4,5]), (1, [1,2]), (2, [0,1,3,4,6]), (3, [0,1,2,3,6]), (4, [1,2,5,6]), (5, [0,2,3,5,6]), (6, [0,2,3,4,5,6]), (7, [0,1,2]), (8, [0,1,2,3,4,5,6]), (9, [0,1,2,3,5,6])]
--digitSegments = Map.fromList[(0, [0,1]), (1, [1,2])]

-- inspired by https://stackoverflow.com/questions/21538903/how-can-i-elegantly-invert-a-maps-keys-and-values
segmentsLeftToDigit :: Map.Map Int [Int] -> Map.Map Int [Int]
segmentsLeftToDigit digitToSegments = Map.fromListWith (++) pairs
    where pairs = [(length vs, [k]) | (k, vs) <- Map.toList digitToSegments]

invert :: (Ord b, Ord a) => Map.Map a b -> Map.Map b a
invert ic = Map.fromList [(v, k) | (k, v) <- Map.toList ic]

headOrEmpty :: [(Int, Int, String)] -> (Int, Int, String)
headOrEmpty [] = (-1, -1, "")
headOrEmpty xs = head xs

{- I had way to litle motivation to do this in haskell. The solution is a sad horrible patchwork of just adding little things to try and make it work without thinking.
and that does not work very well when you are not very proficient in the language... and when it is a recursive solution as well. 

The one positive thing that came from this was that I learned about the 'import Debug.Trace'. That is beyond awesome, I wonder if i have known about that in previous years. I can't recall.
I've left some of the debugging work in comments below, just to highlight how tedious that was. The trace method lets you print in the middle of code, That is so helpful! 

In the end what made the solution work was a weird hack which should not be required in the 'imagined' algorithm. Maybe theres something wrong in my thought process, but it was basically:

1: See if any number is uniquely defined by the words
2: remember that
3: decide on 1 letter to fix in place
4: fix that letter in place and recurse with that and the memory of what letter you know should light up which segments of digits-}

determineRestraint :: Circuits -> Map.Map Int [Int] -> [(String, Int)] -> [(Int, Int, String)]
determineRestraint c mapOfSegmentsLeftToDigit wiring =  sort $ concatMap inspect wiring
    where inspect (wires, thisWireIsThisDigit) = let segmentCounts = Map.findWithDefault [] (placesLeftInWires c wires) mapOfSegmentsLeftToDigit  -- CHANGE 2 replace this length wires
                                     in if (length segmentCounts /= 1 && thisWireIsThisDigit /= -1) || 0 == placesLeftInWires c wires
                                        then [] 
                                        else [(placesLeftInWires c wires, case find (== thisWireIsThisDigit) segmentCounts of Just a -> a   -- CHANGE 4 replace this length wires
                                                                                                                              Nothing -> head segmentCounts, oldWires c wires)]

expandRestraintToFixOptions :: Map.Map Int [Int] -> [(Int, Int, String)] -> [(Int, Char)] 
expandRestraintToFixOptions _ [] = []
expandRestraintToFixOptions ds rs = take 3 $ concatMap expandRestraint rs
    where expandRestraint (_, digit, characters) = if Map.member digit ds then [(head (ds Map.! digit), c) | c <- characters] else []

{- I probably could clean this up a little bit, but since in the end I don't understand why i needed to make determineRestraint return a list it would just be polishing pearls for the swines
... (And that hilarious 'take 3' on the fixOptions is just very indicative of my mindset towards this solution)

So yeah, that I very early, resigned to not understanding my own code is a recipe for distaster-}

solve :: ([(String, Int)], Map.Map Int [Int], Circuits) -> ([(String, Int)], Map.Map Int [Int], Circuits)
solve (o_segments, digits, c) = {-trace ("z = " ++ show (c)) $ -}foldl theFold (segments, digits, c) fixOptions
    where fixOptions = expandRestraintToFixOptions digits $ determineRestraint c (segmentsLeftToDigit digits) segments
          theFold (s1, d1, c1) opt = if isComplete s1 c1 then (s1, d1, c1) else solve (fixSegmentInPlace (segments, digits, c) opt)
          segments = markDeterminedWirings c digits o_segments

{-
 **** Important --- import Debug.Trace is awesome, it allows you to print in the middle of your pure code, truly revolutionary. Have I really missed that in all my years doing haskell AoC? Or is this simply rediscovered knowledge
-}

isComplete :: [(String, Int)] -> Circuits -> Bool
isComplete wiring c = length (Map.keys c) == 7 && all (\(chars) -> Map.member chars (translateDigitMap c)) (map (\(chars,digit) -> sort chars ) wiring)

fixSegmentInPlace :: ([(String, Int)], Map.Map Int [Int], Circuits) -> (Int, Char) -> ([(String, Int)], Map.Map Int [Int], Circuits)
fixSegmentInPlace (wiring, digits, circuits) (segment, character) = (
    wiring,
    Map.map (filter (/= segment)) digits, 
    Map.insert segment character circuits)

{- It does feel like I've forgotten muc habout my haskell knowledge. Or at least it's not readily accessable, i know of cases, guards and lots of stuff, but im not fluent in it so i avoid it.
It hurts to do it like that. But when my brain enters 'I just want it to be over' mode, and its coding things, it is never good.

One positive thing though was the experiences with the '.' operator. Pretty nice, I still don't udnerstand it consiously but at a couple of instances it just felt right to compose those funcitons and the compiler agreed with my syntax, very suprisingly
-}

markDeterminedWirings :: Circuits -> Map.Map Int [Int] -> [(String, Int)] -> [(String, Int)]
markDeterminedWirings cs ds wirings = map update wirings
    where update (wires, isThisDigit) = let  mapOfSegmentsLeftToDigit = Map.findWithDefault [] (placesLeftInWires cs wires) (segmentsLeftToDigit ds)  -- CHANGE 3 replace this length wires
                                        in (wires, if isThisDigit == -1 then (if length mapOfSegmentsLeftToDigit == 1 then head mapOfSegmentsLeftToDigit else -1 ) else isThisDigit)

translateDigitMap :: Circuits -> Map.Map String Int
translateDigitMap c = invert $ Map.map convertSegmentPositionListIntoString digitSegments
    where convertSegmentPositionListIntoString positionList = sort $ map (\x -> c Map.! x) positionList

{- This was just a frustrating experience of patching together things, not quite understanding all the implciations of the code you write and a general unwillingsness to restart. 
Sunken cost fallacy if you will, I started and didn't have much of an plan, AND that is always bad when you're dealing with recursion. I've been here before, you need the code to be clear.
I need to not just be super lazy and do bare minimum thinking.

I should've written tight contained logic blocks, written some simple tests right here in the main code. I did this to some extent but it just all went away when I couldn't debug properly and started patching in thing that might just solve it without me having to understand what was wrong.-}

placesLeftInWires :: Circuits -> String -> Int
placesLeftInWires c wires = length $ filter (\letter -> notElem letter usedUp) wires
    where usedUp = Map.elems c

oldWires :: Circuits -> String -> String
oldWires c wires = filter (\letter -> notElem letter usedUp) wires
    where usedUp = Map.elems c

main = do  
    putStrLn "Start..."  
    contents <- readFile "input/AoC_2021_d8.txt"
    let rowsOfFile = lines contents
    let specialTest = ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"]

    -- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
    -- mapM_ print (filter (\x -> x == 1 || x == 2 || x == 3 || x == 7) $ concatMap (map length . words . last . splitOn "|") rowsOfFile)

    let part1 = length $ filter (\x -> x == 2 || x == 3 || x == 4 || x == 7) $ concatMap (map length . words . last . splitOn "|") rowsOfFile
    putStrLn $ "Result - Part 1: " ++ show (part1)

    let transformed = map (\w-> (w,-1)) $ head $ map (words . head . splitOn "|") specialTest

    let inputs = map (map (\w-> (w,-1))) $ map (words . head . splitOn "|") rowsOfFile
    let solved = map (\row -> solve (row, digitSegments, emptyCircuits)) inputs
    let solutions = map (\(_,_,s) -> s) solved
    let displays = map (map sort . (words . last . splitOn "|")) rowsOfFile
    let mappingAndDisplays = zip displays $ map translateDigitMap solutions
    let toInts = map (\(displays, mapping) -> read (concatMap (\digitInLetters -> show (mapping Map.! digitInLetters)) displays) :: Int) mappingAndDisplays


    --putStrLn $ "transformed: " ++ show (transformed)
    --putStrLn $ "Result - Part 2: " ++ show (digitSegments)
    --putStrLn $ "Result - Part 2: " ++ show (segmentsLeftToDigit digitSegments)
    --putStrLn $ "restraints: " ++ show (determineRestraint emptyCircuits (segmentsLeftToDigit digitSegments) transformed)
    --putStrLn $ "fixOptions: " ++ show (expandRestraintToFixOptions digitSegments $ determineRestraint emptyCircuits (segmentsLeftToDigit digitSegments) transformed)
    --putStrLn $ "fixSegmentInPlace: " ++ show (fixSegmentInPlace (transformed, digitSegments, emptyCircuits) (1, 'a'))
    --putStrLn $ ":\n __+_+_+_+_+__"
    --let (s,d,solvedSolved) = solve (transformed, digitSegments, emptyCircuits)
    --putStrLn $ "solve: " ++ show ((s,d,solvedSolved))
    --putStrLn $ "solve success: " ++ show (isComplete s solvedSolved)

    --putStrLn $ "displays: " ++ show (solved)
    --putStrLn $ "displays: " ++ show (map (\(s,d,c)-> isComplete s c) solved)
    putStrLn $ "toInts: " ++ show (sum toInts)


    {-let (seg0, char0) = head $ expandRestraintToFixOptions digitSegments $ determineRestraint emptyCircuits (segmentsLeftToDigit digitSegments) transformed
    let (segments, digits, circuits) = fixSegmentInPlace (markDeterminedWirings emptyCircuits digitSegments transformed, digitSegments, emptyCircuits) (seg0, char0)
    let (seg, char) = head $ expandRestraintToFixOptions digits $ determineRestraint circuits (segmentsLeftToDigit digits) segments
    let (segments2, digits2, circuits2) = fixSegmentInPlace (markDeterminedWirings circuits digits segments, digits, circuits) (seg, char)

    let bbb = determineRestraint circuits2 (segmentsLeftToDigit digits2) segments2

    let (seg2, char2) = head $ expandRestraintToFixOptions digits2 $ determineRestraint circuits2 (segmentsLeftToDigit digits2) segments2
    let (segments3, digits3, circuits3) = fixSegmentInPlace (markDeterminedWirings circuits2 digits2 segments2, digits2, circuits2) (seg2, char2)
    let (seg3, char3) = head $ expandRestraintToFixOptions digits3 $ determineRestraint circuits3 (segmentsLeftToDigit digits3) segments3
    let (segments4, digits4, circuits4) = fixSegmentInPlace (markDeterminedWirings circuits3 digits3 segments3, digits3, circuits3) (seg3, char3)
    let (seg4, char4) = head $ expandRestraintToFixOptions digits4 $ determineRestraint circuits4 (segmentsLeftToDigit digits4) segments4
    let (segments5, digits5, circuits5) = fixSegmentInPlace (markDeterminedWirings circuits4 digits4 segments4, digits4, circuits4) (seg4, char4)
    let (seg5, char5) = head $ expandRestraintToFixOptions digits5 $ determineRestraint circuits5 (segmentsLeftToDigit digits5) segments5
    let (segments6, digits6, circuits6) = fixSegmentInPlace (markDeterminedWirings circuits5 digits5 segments5, digits5, circuits5) (seg5, char5)
    let (seg6, char6) = head $ expandRestraintToFixOptions digits6 $ determineRestraint circuits6 (segmentsLeftToDigit digits6) segments6
    let (segments7, digits7, circuits7) = fixSegmentInPlace (markDeterminedWirings circuits6 digits6 segments6, digits6, circuits6) (seg6, char6)

    putStrLn $ ":\n ____________"
    putStrLn $ "solve+0:\n " ++ show ((markDeterminedWirings emptyCircuits digitSegments transformed, digitSegments, emptyCircuits))
    putStrLn $ "option+0:\n " ++ show ((seg0, char0))
    putStrLn $ "solve+1:\n " ++ show ((segments, digits, circuits))
    putStrLn $ "option+2:\n  " ++ show ((seg, char))
    putStrLn $ "solve +2:\n  " ++ show ((segments2, digits2, circuits2))

    putStrLn $ ":\n ____________" ++ show bbb

    putStrLn $ "option+3:\n  " ++ show ((seg2, char2))
    putStrLn $ "solve +3:\n  " ++ show ((segments3, digits3, circuits3))
    putStrLn $ "option+4:\n  " ++ show ((seg3, char3))
    putStrLn $ "option+4:\n  " ++ show ((segments4, digits4, circuits4))
    putStrLn $ "option+5:\n  " ++ show ((seg4, char4))
    putStrLn $ "option+5:\n  " ++ show ((segments5, digits5, circuits5))
    putStrLn $ "option+6:\n  " ++ show ((seg5, char5))
    putStrLn $ "option+6:\n  " ++ show ((segments6, digits6, circuits6))
    putStrLn $ "option+6:\n  " ++ show ((seg6, char6))
    putStrLn $ "option+6:\n  " ++ show ((segments7, digits7, circuits7))
    putStrLn $ "option+6:\n  " ++ show ((segments7, digits7, isComplete segments7 circuits7))-}
    putStrLn $ "dghdg:\n  " ++ show (isComplete transformed $ Map.fromList [(0,'c'),(1,'f'),(2,'g'),(3,'b'),(4,'d'),(5,'e'),(6,'a')])  ++ "       " ++ show (Map.fromList [(0,'c'),(1,'f'),(2,'g'),(3,'b'),(4,'d'),(5,'e'),(6,'a')])
    putStrLn $ "Dont forget that this isnt a real solve:\n  "
    


{-
 **** Important --- import Debug.Trace is awesome, it allows you to print in the middle of your pure code, truly revolutionary. 
 Have I really missed that in all my years doing haskell AoC?
Or is this simply rediscovered knowledge?
-}

{- but at least I'm glad it worked in the end-}