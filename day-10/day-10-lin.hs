import Data.Char (digitToInt)
import GHC.Float (roundFloat)
import Data.List (partition)

part2 = map (minimizeSum . solveLin . machineToLin . toMachine) . take 1 . lines
part2' = map (solveLin . machineToLin . toMachine) . take 1 . lines

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

type Matrix = [[Int]]
type FMatrix = [[Float]]

-- we can write all terms as x0*1 + x1*a + x2*b + ...
--  where xi is defined and a,b,.. are variables
--  each term can now be represented as [x0, x1, x2, ...]
type Term = [Int]
-- assigns a value to each variable by its position
--  should be one shorter than the term
type Assignment = [Int]

-- assumes that assigment is complete
eval :: Assignment -> Term -> Int
eval a term = head $ foldr evalStep term a

evalStep :: Int -> Term -> Term
evalStep n (x1:x2:xs) = x1 + n*x2 : xs

-- >> machineToLin . toMachine . head . lines $ x
machineToLin :: Machine -> Matrix
machineToLin (Machine _ buttons joltage) = let
    mapButton n button = if n `elem` button then 1 else 0
    createLine n = map (mapButton n) buttons ++ [joltage !! n]
    in map createLine [0..length joltage - 1]

arbitraryMaximum = 10

-- find values from [0..] for all n variables such that all terms are >= 0 and the sum of terms is minimal
minimizeSum :: [Term] -> [Int]
minimizeSum terms = let
    in map (eval (optimalAssignment terms)) terms

-- optimizing for one variable:
        -- find lowest value such that all terms are >= 0
        -- from there, keep incrementing and determine min value recursively
        -- stop when a term is < 0 or at arbitrary exit value (bad)
optimalAssignment :: [Term] -> [Int]
optimalAssignment ts
    | length (head ts) == 1 = []
    | otherwise = let
    -- only terms containing the variable are relevant
    ts' = filter (\ (_:t:_) -> t /= 0) ts
    findLowestValid tentative
        | all (validTerm . evalStep tentative) ts' = tentative
        | tentative == arbitraryMaximum = -1
        | otherwise = findLowestValid (tentative + 1)
    findHighestValid tentative
        | tentative == arbitraryMaximum = arbitraryMaximum
        | all (validTerm . evalStep tentative) ts' = findHighestValid (tentative + 1)
        | otherwise = tentative - 1

    lowestValid = findLowestValid 0
    highestValid = findHighestValid lowestValid
    tree = [(x, v:opt) | v <- [lowestValid..highestValid],
        let ts'' = map (evalStep v) ts,
        let opt = optimalAssignment ts'',
        let x = sum $ map (eval (v:opt)) ts]
    optimal = if null ts'
        then optimalAssignment (map (evalStep 0) ts)
        else (snd . minimum) tree
    in map fst tree
    where
        validTerm (x:xs) = x >= 0 || any (/= 0) xs

-- solve a system of linear equations
--  returns a term for every component
solveLin :: Matrix -> [Term]
solveLin mat = let
    (fref, free) = gauss (map (map fromIntegral) mat)
    ref = map (map roundFloat) fref
    numFree = length free
    trivialTerms = [[if j == i then 1 else 0 | j <- [0..numFree]] | i <- [1..numFree]]
    toTerm line = let
        buildTerm [x] _ = [x]
        buildTerm (x:xs) n
            | n `elem` free = -x : buildTerm xs (n+1)
            | otherwise = buildTerm xs (n+1)
        in reverse (buildTerm line 0)
    in map toTerm ref ++ trivialTerms

-- transform expanded coefficient matrix into RREF
--  return matrix along with column indices of free variables
gauss :: FMatrix -> (FMatrix, [Int])
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
gaussStep :: FMatrix -> Int -> (FMatrix, Bool)
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
