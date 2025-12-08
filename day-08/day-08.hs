import Data.List(partition)

part1 = process1 . parse
part2 = process2 . parse

type Coord = (Int, Int, Int)
parse :: String -> [Coord]
parse = let
    parseLine line = let
        (x, yz) = span (/= ',') line
        (y, z) = span (/= ',') (drop 1 yz)
        in (read x, read y, read . drop 1 $ z)
    in map parseLine . lines

process1 :: [Coord] -> Int -> Int
process1 coords n = let
    (circuits, connections) = prepare coords
    connected = connectN n circuits connections
    largestCircuits = take 3 . sort (<) . map length $ connected
    in product largestCircuits

process2 :: [Coord] -> Int
process2 coords = let
    (circuits, connections) = prepare coords
    ((x1,_,_), (x2,_,_)) = connectAll circuits connections
    in x1 * x2

prepare :: [Coord] -> ([[Coord]], [(Coord, Coord)])
prepare unsortedCoords = let
    coords = sort (>) unsortedCoords
    distSquared ((a,b,c), (x,y,z)) = (a-x)^2 + (b-y)^2 + (c-z)^2
    combos = combinations coords
    measured = zip combos (map distSquared combos)
    sorted = sort (\ x y -> snd x > snd y) measured
    connections = map fst sorted
    in ([[c] | c <- coords], connections)

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
    circuits' = addConnection circuits conn
    in connectN (n-1) circuits' remConns

-- gives us the last connection required
connectAll :: [[Coord]] -> [(Coord, Coord)] -> (Coord, Coord)
connectAll circuits conns = let
    ([conn], remConns) = splitAt 1 conns
    circuits' = addConnection circuits conn
    in if null (drop 1 circuits') then conn else connectAll circuits' remConns

addConnection :: [[Coord]] -> (Coord, Coord) -> [[Coord]]
addConnection circuits conn = let
    c1 = head . filter (fst conn `elem`) $ circuits
    c2 = head . filter (snd conn `elem`) $ circuits
    connected = head c1 == head c2
    circuits' = [c' | c <- circuits, c /= c2, let c' = if c /= c1 then c else c ++ c2]
    in if connected then circuits else circuits'
