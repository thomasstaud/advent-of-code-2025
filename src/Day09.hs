module Day09(main) where

main = do
    file <- readFile "inputs/09.txt"
    let input = parse file
    print $ part1 input
    print $ part2 input

part1 = process1
part2 = process2

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


-- these values were found by taking a good hard look at the input data
(tx, ty) = (94918, 48430)
(bx, by) = (94918, 50338)

process2 :: [(Int, Int)] -> Int
process2 coords = let
    getSize (x1, y1) (x2, y2) = (abs (x1-x2)+1) * (abs (y1-y2)+1)
    b = border $ coords ++ take 1 coords
    -- top
    yTop = (snd . minimum) (filter (\ (x, y) -> x == tx) b)
    xTop = (fst . minimum) (filter (\ (x, y) -> y == yTop) b)
    yTop' = (snd . maximum) (filter (\ (x, y) -> x == xTop) coords)
    sizeTop = getSize (tx, ty) (xTop, yTop')
    -- bottom
    yBtm = (snd . maximum) (filter (\ (x, y) -> x == bx) b)
    xBtm = (fst . minimum) (filter (\ (x, y) -> y == yBtm) b)
    yBtm' = (snd . minimum) (filter (\ (x, y) -> x == xBtm) coords)
    sizeBtm = getSize (bx, by) (xBtm, yBtm')
    in max sizeTop sizeBtm

-- needs the first element to be copied to the end of the list for wrapping
border :: [(Int, Int)] -> [(Int, Int)]
border [_] = []
border ((x1, y1):(x2, y2):cs) = let
    line = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2], (x, y) /= (x2, y2)]
    in line ++ border ((x2, y2):cs)



-- utility (could be removed)
getBorder str = border $ coords ++ take 1 coords
    where
        coords = parse str

-- call like this:
--  x <- readFile "input"
--  putStr $ plotSectors (parse x) 80 30
--  putStr $ plotSectors (getBorder x) 80 30
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
