import Data.Char (digitToInt)
import Data.Bits (Bits(..))

part1 = sum . map (minLightPresses . toMachine) . lines
part2 = map (minJoltPresses . toMachine) . lines

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

-- determine number of required button presses for the lights of one machine
minLightPresses :: Machine -> Int
minLightPresses (Machine lights buttons _) = minimum . map length $ lightsCombinations buttons lights

-- determine number of required button presses for the joltage of one machine
minJoltPresses :: Machine -> Int
minJoltPresses (Machine _ buttons joltage) = let
    -- takes the LSB from every joltage number, representing them as lights
    constructLights :: Joltage -> Lights
    constructLights [] = 0
    constructLights (x:xs) = x .&. 1 + shift (constructLights xs) 1
    combinations = lightsCombinations buttons (constructLights joltage)
    -- checks one combination for its minimum score
    eval :: [Button] -> Int
    eval [] = 1_000_000 -- impossible
    eval combo = let
        joltage' = [(j - length (filter (`testBit` i) combo)) `div` 2 | (j, i) <- zip joltage [0..]]
        val = length combo + 2 * minJoltPresses (Machine 0 buttons joltage')
        in if any (/= 0) joltage' then val else 0
    in minimum $ map eval combinations

lightsCombinations :: [Button] -> Lights -> [[Button]]
lightsCombinations buttons lights  = let
    -- order of buttons doesn't matter, pressing a button twice does nothing
    possibilities = 2 ^ length buttons
    -- filters buttons with a binary mask
    toButtons :: Int -> [Button]
    toButtons n = [b | (b, i) <- zip buttons [0..], testBit n i]
    isValid :: [Button] -> Bool
    isValid bs = foldr1 xor bs == lights
    in filter isValid (map toButtons [1..possibilities-1])
