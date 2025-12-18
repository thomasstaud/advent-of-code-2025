{-# LANGUAGE NumericUnderscores #-}
module Day10(part1, part2) where

import Data.Char (digitToInt)
import Data.Bits (Bits(..))
import qualified Data.Map as M

main = do
    input <- readFile "input.txt"
    print $ part1 input
    print $ part2 input

part1 = sum . map (minLightPresses . toMachine) . lines
part2 = sum . map (minJoltPresses . toMachine) . lines

type Lights = Int       -- bitmask
type Button = Int       -- bitmask
type Joltage = [Int]    -- actual values
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
    parseJoltage ('{':str) = read ("[" ++ (reverse . drop 1 . reverse) str ++ "]")
    in Machine (parseLights lightStr) (map parseButton buttonStrs) (parseJoltage joltStr)


-- find required button presses for the *lights* of one machine
minLightPresses :: Machine -> Int
minLightPresses (Machine lights buttons _) = let
    possibilities = 2 ^ length buttons
    -- filter buttons with a binary mask
    filterButtons :: Int -> [Button]
    filterButtons n = [b | (b, i) <- zip buttons [0..], testBit n i]

    search :: Int -> Int
    search n = let
        valid = [() | mask <- [0..possibilities-1], popCount mask == n, let bs = filterButtons mask, foldr xor 0 bs == lights]
        in if null valid then search (n+1) else n
    in search 0


impossible = 1_000_000
-- use memoization to avoid duplicate evaluation
type Memo = M.Map Joltage Int
type LightMap = M.Map Lights [[Button]]

-- find required button presses for the *joltage* of one machine
minJoltPresses :: Machine -> Int
minJoltPresses (Machine _ buttons joltage) = fst $ minimize M.empty joltage
    where
        lightMap = buildLightMap buttons

        minimize :: Memo -> Joltage -> (Int, Memo)
        minimize memo j
            | all (== 0) j = (0, memo) -- success
            | any (< 0) j = (impossible, memo)
            | Just v <- M.lookup j memo = (v, memo)
            | otherwise = let
                lights = constructLights j
                combos = M.findWithDefault [] lights lightMap
                (v, memo') = foldr (eval j) (impossible, memo) combos
            in (v, M.insert j v memo')
        
        -- checks one combination for its minimum score
        eval :: Joltage -> [Button] -> (Int, Memo) -> (Int, Memo)
        eval j combo (best, memo) = let
            j' = [(j - length (filter (`testBit` i) combo)) `div` 2 | (j, i) <- zip j [0..]]
            (v, memo') = minimize memo j'
            in (min best (length combo + 2 * v), memo')


buildLightMap :: [Button] -> LightMap
buildLightMap buttons = let
    possibilities = 2 ^ length buttons
    -- filter buttons with a binary mask
    filterButtons :: Int -> [Button]
    filterButtons n = [b | (b, i) <- zip buttons [0..], testBit n i]

    combinations = [(lights, [bs]) | mask <- [0..possibilities-1], let bs = filterButtons mask, let lights = foldr xor 0 bs]
    in M.fromListWith (++) combinations

-- takes the LSB from every joltage number, representing them as lights
constructLights :: Joltage -> Lights
constructLights [] = 0
constructLights (x:xs) = x .&. 1 + shift (constructLights xs) 1
