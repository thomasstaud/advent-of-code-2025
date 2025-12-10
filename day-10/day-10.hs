import Data.Char (digitToInt)

part1 = sum . map (process . toMachine) . lines

type Lights = [Bool]
type Button = [Int]
type Joltage = [Int]
data Machine = Machine Lights [Button] Joltage deriving Show

toMachine :: String -> Machine
toMachine line = let
    -- split line
    (lightStr:tail) = words line
    ([joltStr], buttonStrs) = splitAt 1 . reverse $ tail
    -- parse strings
    parseLights (c:str) = case c of
        '[' -> parseLights str
        '.' -> False : parseLights str
        '#' -> True : parseLights str
        ']' -> []
    parseButton (c:str) = case c of
        '(' -> parseButton str
        ',' -> parseButton str
        ')' -> []
        _ -> digitToInt c : parseButton str
    -- not needed for part 1
    parseJoltage _ = []
    in Machine (parseLights lightStr) (map parseButton buttonStrs) (parseJoltage joltStr)

-- determine number of required button presses for one machine
process :: Machine -> Int
process (Machine lights buttons _) = let
    -- optimization idea: start from lights as well and meet in the middle
    --  (should halve the exponentiation of states)
    match n leftStates rightStates
        | any (`elem` leftStates) rightStates = n
        | even n = match (n+1) leftStates (step buttons rightStates)
        | otherwise = match (n+1) (step buttons leftStates) rightStates
    in match 0 [replicate (length lights) False] [lights]

-- press each button for every set of lights
step :: [Button] -> [Lights] -> [Lights]
step buttons lights = concatMap (\ b -> map (press b) lights) buttons

press :: Button -> Lights -> Lights
press b lights = [l' | (l, i) <- zip lights [0..], let l' = if i `elem` b then not l else l]
