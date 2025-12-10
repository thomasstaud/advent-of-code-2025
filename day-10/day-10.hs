import Data.Char (digitToInt)

part1 = sum . map (minLightPresses . toMachine) . lines
part2 = map (minJoltPresses . toMachine) . lines

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
    parseJoltage ('{':str) = parseJoltage str
    parseJoltage str = case span (/= ',') str of
        (numStr, ',':tail) -> read numStr : parseJoltage tail
        -- remove closing bracket
        (tail, []) -> [read . reverse . drop 1 . reverse $ tail]
    in Machine (parseLights lightStr) (map parseButton buttonStrs) (parseJoltage joltStr)

-- determine number of required button presses for the lights of one machine
minLightPresses :: Machine -> Int
minLightPresses (Machine lights buttons _) = let
    -- start from both start & goal and meet in the middle for optimization
    match n leftStates rightStates
        | any (`elem` leftStates) rightStates = n
        | even n = match (n+1) leftStates (step flipLights buttons rightStates)
        | otherwise = match (n+1) (step flipLights buttons leftStates) rightStates
    in match 0 [replicate (length lights) False] [lights]

-- determine number of required button presses for the joltage of one machine
minJoltPresses :: Machine -> Int
minJoltPresses (Machine _ buttons joltage) = let
    -- start from both start & goal and meet in the middle for optimization
    match n leftStates rightStates
        | any (`elem` leftStates) rightStates = n
        | even n = match (n+1) leftStates (step incJolt buttons rightStates)
        | otherwise = match (n+1) (step incJolt buttons leftStates) rightStates
    in match 0 [replicate (length joltage) 0] [joltage]

-- press each button for every state
step :: (Button -> state -> state) -> [Button] -> [state] -> [state]
step press buttons lights = concatMap (\ b -> map (press b) lights) buttons

flipLights :: Button -> Lights -> Lights
flipLights b lights = [l' | (l, i) <- zip lights [0..], let l' = if i `elem` b then not l else l]

incJolt :: Button -> Joltage -> Joltage
incJolt b joltage = [j' | (j, i) <- zip joltage [0..], let j' = if i `elem` b then j+1 else j]



-- alternative solution:

-- transform part 2 into system of linear equations
--  ex 1: (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--         x1  x2    x3  x4    x5    x6    b

-- x5+x6=3
-- x2+x6=5
-- x3+x4+x5=4
-- x1+x2+x4=7

-- matrix form:
--  0 0 0 0 1 1 | 3    1 1 0 1 0 0 | 7    1 0 0 1 0-1 | 7
--  0 1 0 0 0 1 | 5 -> 0 1 0 0 0 1 | 5 -> 0 1 0 0 0 1 | 5
--  0 0 1 1 1 0 | 4    0 0 1 1 1 0 | 4    0 0 1 1 0-1 | 4
--  1 1 0 1 0 0 | 7    0 0 0 0 1 1 | 3    0 0 0 0 1 1 | 3

-- x6 = a
-- x5 = 3-a
-- x4 = b
-- x3 = 4+a-b
-- x2 = 5-a
-- x1 = 7+a-b

--  wobei xi aus [0..],
--  sodass: x1+x2+x3+x4+x5+x6 minimal
--      hier z.B. a = 0, b = 4
--      d.h. 3+5+0+4+3+0 = 15
--     aber: 1+3+0+3+1+2 = 10 ist besser :'(

type Matrix = [[Int]]

-- solve a system of linear equations
solveLin :: Matrix -> [Int]
solveLin mat = let
    ref = gauss mat
    in undefined

-- transform expanded coefficient matrix into REF
gauss :: Matrix -> Matrix
gauss mat = undefined
