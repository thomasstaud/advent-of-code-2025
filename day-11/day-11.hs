import Data.List (partition)

part1 = process "you" [] . parse
part2 = process "svr" ["dac", "fft"] . parse

data Device = Device String [String] deriving (Eq, Show)

parse :: String -> [Device]
parse str = let
    toDevice line = case span (/= ':') line of
        (name, ':':outputs) -> Device name (words outputs)
    in map toDevice $ lines str

end = "out"
-- determine how many paths from 'start' to end containing all devices in 'passes' there are
process :: String -> [String] -> [Device] -> Int
process start passes devices = let
    passDevices = filter (\ (Device name _) -> name `elem` passes) devices
    isEndDevice (Device _ outputs) = end `elem` outputs
    isValidPath path = isEndDevice (head path) && all (`elem` path) passDevices
    loop [] = 0
    loop paths = let
        paths' = concatMap (step devices) paths
        -- filter paths that are done
        done = filter isValidPath paths'
        in length done + loop paths'
    startPaths = filter (\ (Device name _) -> name == start) devices
    in loop [startPaths]

type Path = [Device]
-- add all possible next devices to the front of the path
--  cycling paths are removed
step :: [Device] -> Path -> [Path]
step devices path@((Device _ outputs):_) = map (:path) (filter deviceFilter devices)
    where deviceFilter d@(Device name _) = name `elem` outputs && d `notElem` path
