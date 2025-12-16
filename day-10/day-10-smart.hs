import Data.Char (digitToInt)
import Data.Bits (Bits(..))
import qualified Data.Map.Strict as M

main = do
    input <- readFile "heavy.txt"
    print $ part2 input

part1 = sum . map (minLightPresses . toMachine) . lines
part2 = sum . map (minJoltPresses . toMachine) . lines

type Lights = Int -- binary encoded
type Button = Int -- binary encoded
type Joltage = [Int]
data Machine = Machine Lights [Button] Joltage deriving Show

toMachine :: String -> Machine
toMachine line = let
    -- split line
    (lightStr:tail) = words line
    (buttonStrs, [joltStr]) = splitAt (length tail - 1) tail
    -- parse strings
    parseLights (c:str) = case c of
        '[' -> parseLights str
        '.' -> shift (parseLights str) 1
        '#' -> 1 + shift (parseLights str) 1
        ']' -> 0
    parseButton (c:str) = case c of
        '(' -> parseButton str
        ',' -> parseButton str
        ')' -> 0
        _ -> bit (digitToInt c) + parseButton str
    parseJoltage ('{':str) = parseJoltage str
    parseJoltage str = case span (/= ',') str of
        (numStr, ',':tail) -> read numStr : parseJoltage tail
        -- remove closing bracket
        (tail, []) -> [read (takeWhile (/= '}') tail)]
    in Machine (parseLights lightStr) (map parseButton buttonStrs) (parseJoltage joltStr)

-- find required button presses for the *lights* of one machine
minLightPresses :: Machine -> Int
minLightPresses (Machine lights buttons _) = minimum . map length $ lightsCombinations buttons lights

impossible = 1_000_000
-- use memoization to avoid duplicate evaluation
type Memo     = M.Map Joltage Int

-- find required button presses for the *joltage* of one machine
minJoltPresses :: Machine -> Int
minJoltPresses (Machine _ buttons joltage) = fst $ minimize M.empty joltage
    where
        -- TODO: implement light map here

        minimize :: Memo -> Joltage -> (Int, Memo)
        minimize memo j
            | all (== 0) j = (0, memo) -- success
            | any (< 0) j = (impossible, memo)
            | Just v <- M.lookup j memo = (v, memo)
            | otherwise = let
            combos = lightsCombinations buttons (constructLights j)
            (v, memo') = foldr (eval j) (impossible, memo) combos
            in (v, M.insert j v memo)
        
        -- checks one combination for its minimum score
        eval :: Joltage -> [Button] -> (Int, Memo) -> (Int, Memo)
        eval j combo (best, memo) = let
            subtractHits x i = let
                hits = length [() | b <- combo, testBit b i]
                in (x - hits) `div` 2
            j' = zipWith subtractHits j [0..]
            (v, memo') = minimize memo j'
            in (min best (length combo + 2 * v), memo')



-- j' = [(j - length (filter (`testBit` i) combo)) `div` 2 | (j, i) <- zip j [0..]]

lightsCombinations :: [Button] -> Lights -> [[Button]]
lightsCombinations buttons lights  = let
    -- order of buttons doesn't matter, pressing a button twice does nothing
    possibilities = 2 ^ length buttons
    -- filters buttons with a binary mask
    toButtons :: Int -> [Button]
    toButtons n = [b | (b, i) <- zip buttons [0..], testBit n i]
    isValid :: [Button] -> Bool
    isValid bs = foldr xor 0 bs == lights
    in filter isValid (map toButtons [0..possibilities-1])

-- takes the LSB from every joltage number, representing them as lights
constructLights :: Joltage -> Lights
constructLights [] = 0
constructLights (x:xs) = x .&. 1 + shift (constructLights xs) 1
