import Data.List (partition)

part1 = process "you" [] . parse
part2 = process "svr" ["dac", "fft"] . parse
-- 399 (too low)

data Device = Device { name :: String, outputs :: [String] } deriving (Eq, Show)

parse :: String -> [Device]
parse str = let
    toDevice line = case span (/= ':') line of
        (name, ':':outputs) -> Device name (words outputs)
    in map toDevice $ lines str

end = "out"
-- determine how many paths from 'start' to end containing all devices in 'visits' there are
process :: String -> [String] -> [Device] -> Int
process start visits devices = let
    visitDevices = filter (\ (Device name _) -> name `elem` visits) devices
    isEndDevice (Device _ outputs) = end `elem` outputs
    isValidPath path = isEndDevice (head path) && all (`elem` path) visitDevices
    updateVisits (State p c v) = let
        n = name (head p)
        v' = if n `elem` visits && n `notElem` v then n:v else v
        in State p c v'
    loop [] = 0
    loop states = let
        states' = concatMap (step devices) states
        -- filter states that are done
        done = filter (\ (State p _ _) -> isValidPath p) states'
        -- merge states
        --  update visits
        updated = map updateVisits states'
        --  merge
        merged = mergeStates updated
        in length done + loop merged
    startPaths = filter (\ (Device name _) -> name == start) devices
    in loop [State [p] 0 [] | p <- startPaths]

-- add all possible next devices to the front of the path
--  cycling paths are removed
step :: [Device] -> State -> [State]
step devices (State path c v) = let
    ((Device _ outputs):_) = path
    deviceFilter d@(Device name _) = name `elem` outputs && d `notElem` path
    paths = map (:path) (filter deviceFilter devices)
    in map (\ p -> State p c v) paths

-- 1) seen devices 2) cardinality 3) encoutered visits
data State = State [Device] Int [String] deriving Show

-- used for merging
instance Eq State where
    (==) (State p1 _ v1) (State p2 _ v2) = let
        -- the first elements must be the same (i think this could be significantly less strict)
        cond1 = head p1 == head p2
        -- every element of v1 must be in v2 and vice versa
        cond2 = all (`elem` v1) v2 && all (`elem` v2) v1
        in cond1 && cond2

-- pretty sure this could be way more thorough
mergeStates :: [State] -> [State]
mergeStates [] = []
mergeStates (s@(State p c v):states) = let
    (mergeable, rest) = partition (== s) states
    -- merge mergeable into s
    cardinality = sum $ map (\ (State _ c _) -> c) mergeable
    s' = State p (c+cardinality) v
    in s' : mergeStates rest
