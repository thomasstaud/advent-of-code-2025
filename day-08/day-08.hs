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

process :: [Coord] -> Int -> [Int]
process unsortedCoords n = let
    coords = sort (>) unsortedCoords
    manhattan ((a,b,c), (x,y,z)) = abs (a-x) + abs (b-y) + abs (c-z)
    combos = combinations coords
    measured = zip combos (map manhattan combos)
    sorted = sort (\ x y -> snd x > snd y) measured
    connections = map fst sorted
    circuits = connectN n [[c] | c <- coords] connections
    largestCircuits = take 3 . sort (<) . map length $ circuits
    in largestCircuits

sort :: (a -> a -> Bool) -> [a] -> [a]
sort cmp [] = []
sort cmp (x:xs) = let
    (lt, gt) = partition (cmp x) xs
    in sort cmp lt ++ [x] ++ sort cmp gt

combinations :: [Coord] -> [(Coord, Coord)]
combinations coords = [(a, b) | a <- coords, b <- dropWhile (<=a) coords]

connectN :: Int -> [[Coord]] -> [(Coord, Coord)] -> [[Coord]]
connectN 0 circuits _ = circuits
connectN n circuits conns = let
    ([conn], remConns) = splitAt 1 conns
    (circuits', updated) = addConnection circuits conn
    n' = if updated then n-1 else n
    in connectN n' circuits' remConns

addConnection :: [[Coord]] -> (Coord, Coord) -> ([[Coord]], Bool)
addConnection circuits conn = let
    c1 = head . filter (fst conn `elem`) $ circuits
    c2 = head . filter (snd conn `elem`) $ circuits
    connected = head c1 == head c2
    circuits' = [c' | c <- circuits, c /= c2, let c' = if c /= c1 then c else c ++ c2]
    in if connected then (circuits, False) else (circuits', True)
