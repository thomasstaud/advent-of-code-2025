import Data.List(partition)

part1 = process . parse

type Coord = (Int, Int, Int)
parse :: String -> [Coord]
parse = let
    parseLine line = let
        (x, yz) = span (/= ',') line
        (y, z) = span (/= ',') (drop 1 yz)
        in (read x, read y, read . drop 1 $ z)
    in map parseLine . lines

process :: [Coord] -> Int -> [[Coord]]
process unsortedCoords n = let
    coords = sort (>) unsortedCoords
    manhattan ((a,b,c), (x,y,z)) = abs (a-x) + abs (b-y) + abs (c-z)
    combos = combinations coords
    measured = zip combos (map manhattan combos)
    sorted = sort (\ x y -> snd x > snd y) measured
    connections = take n sorted
    circuits = collectCircuits (map fst connections) coords
    largestCircuits = take 3 . sort (<) . map length $ circuits
    in circuits

sort :: (a -> a -> Bool) -> [a] -> [a]
sort cmp [] = []
sort cmp (x:xs) = let
    (lt, gt) = partition (cmp x) xs
    in sort cmp lt ++ [x] ++ sort cmp gt

combinations :: [Coord] -> [(Coord, Coord)]
combinations coords = [(a, b) | a <- coords, b <- dropWhile (<=a) coords]

collectCircuits :: [(Coord, Coord)] -> [Coord] -> [[Coord]]
collectCircuits _ [] = []
collectCircuits conns (c : coords) = let
    (inside, outside) = partition (\ x -> (c, x) `elem` conns) coords
    in (c : inside) : collectCircuits conns outside
