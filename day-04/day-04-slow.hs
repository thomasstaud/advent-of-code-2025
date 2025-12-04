part1 = process . parse

type Coords = (Int, Int)

-- list of paper stack coords
parse :: String -> [Coords]
parse str = [(x, y) | (line, y) <- zip (lines str) [0..], (c, x) <- zip line [0..], c == '@']

-- number of accessible paper stacks
process :: [Coords] -> Int
process coords = length . filter (isAccessible coords) $ coords

isAccessible :: [Coords] -> Coords -> Bool
isAccessible coords c@(x, y) = 4 > (length . filter (isAdjacent c)) coords

isAdjacent :: Coords -> Coords -> Bool
isAdjacent (x1, y1) (x2, y2) = let
    xDiff = abs (x1 - x2)
    yDiff = abs (y1 - y2)
    in xDiff <= 1 && yDiff <= 1 && (xDiff == 1 || yDiff == 1)
