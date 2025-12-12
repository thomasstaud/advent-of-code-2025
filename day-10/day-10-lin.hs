import Data.Char (digitToInt)
import Data.List (partition)

part2 = map (minimizeSum . solveLin . machineToLin . toMachine) . lines

-- used for testing
f = minimizeSum . solveLin . machineToLin . toMachine
g = solveLin . machineToLin . toMachine

type Lights = [Bool]
type Button = [Double]
type Joltage = [Double]
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
        _ -> fromIntegral(digitToInt c) : parseButton str
    parseJoltage ('{':str) = parseJoltage str
    parseJoltage str = case span (/= ',') str of
        (numStr, ',':tail) -> read numStr : parseJoltage tail
        -- remove closing bracket
        (tail, []) -> [read . reverse . drop 1 . reverse $ tail]
    in Machine (parseLights lightStr) (map parseButton buttonStrs) (parseJoltage joltStr)



-- transform into system of linear equations
--  ex 1: (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--         x1  x2    x3  x4    x5    x6    b
-- x5+x6=3
-- x2+x6=5
-- x3+x4+x5=4
-- x1+x2+x4=7

-- matrix form:
--  0 0 0 0 1 1 | 3    1 1 0 1 0 0 | 7    1 0 0 1 0-1 | 2
--  0 1 0 0 0 1 | 5 -> 0 1 0 0 0 1 | 5 -> 0 1 0 0 0 1 | 5
--  0 0 1 1 1 0 | 4    0 0 1 1 1 0 | 4    0 0 1 1 0-1 | 1
--  1 1 0 1 0 0 | 7    0 0 0 0 1 1 | 3    0 0 0 0 1 1 | 3

-- x6 = a       [ 0, 1, 0] (trivial)
-- x5 = 3-a     [ 3,-1, 0]
-- x4 = b       [ 0, 0, 1] (trivial)
-- x3 = 1+a-b   [ 1, 1,-1]
-- x2 = 5-a     [ 5,-1, 0]
-- x1 = 2+a-b   [ 2, 1,-1]

--  where a,b from [0..] such that xi in [0..],
--  and x1+x2+x3+x4+x5+x6 minimal <- brute force?
--      here e.g. a = 3, b = 4
--      i.e. 1+2+0+4+0+3 = 10

type Matrix = [[Double]]

-- we can write all terms as x0*1 + x1*a + x2*b + ...
--  where xi is defined and a,b,.. are variables
--  each term can now be represented as [x0, x1, x2, ...]
type Term = [Double]
-- assigns a value to each variable by its position
--  should be one shorter than the term
type Assignment = [Double]

-- assumes that assigment is complete
eval :: Assignment -> Term -> Double
eval a term = head $ foldl evalStep term a

evalStep :: Term -> Double -> Term
evalStep (x1:x2:xs) n = x1 + n*x2 : xs

-- >> machineToLin . toMachine . head . lines $ x
machineToLin :: Machine -> Matrix
machineToLin (Machine _ buttons joltage) = let
    mapButton n button = if fromIntegral n `elem` button then 1 else 0
    createLine n = map (mapButton n) buttons ++ [joltage !! n]
    in map createLine [0..length joltage - 1]

{- heuristic strategy:
    for every variable, plug in the values from 0 to the maximum possible and see what works best
-}
minimizeSum :: ([Term], [Double]) -> Double
minimizeSum (terms, maxes) = let
    len = length (head terms)-1
    nums = [[0..maxes !! i] | i <- [0..len-1]]
    -- append every x to every y
    combo :: [Double] -> [[Double]] -> [[Double]]
    combo xs ys = [x:y | x <- xs, y <- ys]
    assignments = foldr combo [[]] nums
    evalAssignment :: Assignment -> Double
    evalAssignment a = let
        ts = map (eval a) terms
        valid = all (\ t -> t >= 0 && t - fromIntegral (round t) < 0.01) ts
        in if valid then sum ts else 100000 -- could not find a solution
    in minimum (map evalAssignment assignments)

-- solve a system of linear equations
--  returns a term for every component and a maximum value for every free variable
solveLin :: Matrix -> ([Term], [Double])
solveLin mat = let
    (ref, free) = gauss mat
    numFree = length free
    trivialTerms = [[if j == i then 1 else 0 | j <- [0..numFree]] | i <- [1..numFree]]
    toTerm line = let
        buildTerm [x] _ = [x]
        buildTerm (x:xs) n
            | n `elem` free = -x : buildTerm xs (n+1)
            | otherwise = buildTerm xs (n+1)
        in reverse (buildTerm line 0)
    max c = minimum . map last $ filter (\ line -> line !! c == 1) mat
    in (map toTerm ref ++ trivialTerms, map max free)

-- transform expanded coefficient matrix into RREF
--  return matrix along with column indices of free variables
gauss :: Matrix -> (Matrix, [Int])
gauss mat = let
    step mat n
        | length (head mat) == n+1 = (mat, [])
        | otherwise = let
            (mat', hasPivot) = gaussStep mat n
            (mat'', freeVars) = step mat' (n+1)
            in (mat'', [n | not hasPivot] ++ freeVars)
    in step mat 0

-- perform gaussian elimination in column n
--  returns the updated matrix and a bool whether the column has a pivot
--  if it has no pivot, the matrix does not change
gaussStep :: Matrix -> Int -> (Matrix, Bool)
gaussStep mat n = let
    -- 1. move a row with no 0 in the nth column to the nth position
    pivotToTop = let
        (above, row : below) = span (\ line -> (line !! n) == 0) bottom
        -- 2. divide that row by the value of the first element
        row' = map (/ row !! n) row
        in top ++ row' : above ++ below
    -- 3. eliminate all other values in that column
    colEliminated = let
        ([(line, _)], lines') = partition (\ (l, i) -> i == n) (zip pivotToTop [0..])
        lines = map fst lines'
        -- if val is not 1, we have a problem
        val = line !! n
        elim l = zipWith (\ mji m1i -> mji - m1i * (l !! n)) l line
        in line : map elim lines
    (top, bottom) = splitAt n mat
    hasPivot = any (\ line -> (line !! n) /= 0) bottom
    mat' = if hasPivot then colEliminated else mat
    in (mat', hasPivot)
