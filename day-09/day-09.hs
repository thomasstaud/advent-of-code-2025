part1 = process1 . parse
part2 = process2 . parse

parse :: String -> [(Int, Int)]
parse str = let
    parseLine line = let
        (n1, n2) = span (/= ',') line
        in (read n1, read (drop 1 n2))
    in map parseLine . lines $ str

process1 :: [(Int, Int)] -> Int
process1 coords = let
    getSize (x1, y1) (x2, y2) = (abs (x1-x2)+1) * (abs (y1-y2)+1)
    in maximum [size | c1 <- coords, c2 <- coords, let size = getSize c1 c2]

process2 :: [(Int, Int)] -> Int
process2 coords = let
    validTiles = fill . border $ coords ++ take 1 coords
    getSize (x1, y1) (x2, y2) = (abs (x1-x2)+1) * (abs (y1-y2)+1)
    in maximum [size | c1 <- coords, c2 <- coords, isValid validTiles c1 c2, let size = getSize c1 c2]

-- needs the first element to be copied to the end of the list for wrapping
border :: [(Int, Int)] -> [(Int, Int)]
border [_] = []
border ((x1, y1):(x2, y2):cs) = let
    line = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2], (x, y) /= (x2, y2)]
    in line ++ border ((x2, y2):cs)

-- this is estimated to take at least 10 hours
fill :: [(Int, Int)] -> [(Int, Int)]
fill coords = let
    xMax = maximum . map fst $ coords
    yMax = maximum . map snd $ coords
    -- findStart doesn't really work
    --  we need to find a start point manually
    findStart (x, y) crossed
        | crossed && (x, y) `notElem` coords = (x, y)
        | x == xMax = findStart (0, y+1) False
        | crossed = findStart (x+1, y) False
        | otherwise = findStart (x+1, y) ((x,y) `elem` coords)
    start = findStart (0, 1) False
    bucketFill (x, y) filled
        | (x, y) `elem` filled = filled
        | 0 > x || x > xMax || 0 > y || y > yMax = error ("invalid start point -> "++show x++" "++show y)
        | otherwise = let
            filled' = (x, y):filled
            adj = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
            in foldr bucketFill filled' adj
    -- start points were estimated manually (with great care)
    in bucketFill (97616, 51573) coords -- input
    -- in bucketFill (10,2) coords -- example

plotSectors :: [(Int, Int)] -> Int -> Int -> String
plotSectors coords cols rows = let
    xMax = (maximum . map fst $ coords) + 1
    yMax = (maximum . map snd $ coords) + 1
    (sectorX, sectorY) = (xMax `div` cols, yMax `div` rows)
    toSector (x, y) = (x `div` sectorX, y `div` sectorY)
    filledSectors = map toSector coords
    -- if any tiles in sector are filled, plot '#' else '.'
    plotSector x y = if (x, y) `elem` filledSectors then '#' else '.'
    in concatMap ('\n':) [[s | x <- [0..cols], let s = plotSector x y] | y <- [0..rows]]

isValid :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
isValid validTiles (x1, y1) (x2, y2) = let
    spannedTiles = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
    in all (`elem` validTiles) spannedTiles
