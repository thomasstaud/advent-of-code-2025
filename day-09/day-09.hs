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
    -- validTiles = fill . border $ coords ++ take 1 coords
    validTiles = undefined
    getSize (x1, y1) (x2, y2) = (abs (x1-x2)+1) * (abs (y1-y2)+1)
    in maximum [size | c1 <- coords, c2 <- coords, isValid validTiles c1 c2, let size = getSize c1 c2]

-- needs the first element to be copied to the end of the list for wrapping
border :: [(Int, Int)] -> [(Int, Int)]
border [_] = []
border ((x1, y1):(x2, y2):cs) = let
    line = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2], (x, y) /= (x2, y2)]
    in line ++ border ((x2, y2):cs)

fill :: [(Int, Int)] -> (Int, Int)
fill coords = let
    findStart (x, y) crossed
        | crossed && (x, y) `notElem` coords = (x, y)
        | otherwise = findStart (x+1, y+1) ((x,y) `elem` coords)
    start = findStart (0, 0) False 
    bucketFill (x, y) filled = undefined
    in bucketFill start coords

isValid :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
isValid validTiles (x1, y1) (x2, y2) = let
    spannedTiles = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
    in all (`elem` validTiles) spannedTiles
