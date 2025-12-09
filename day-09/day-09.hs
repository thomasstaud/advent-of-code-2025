part1 = process . parse

parse :: String -> [(Int, Int)]
parse str = let
    parseLine line = let
        (n1, n2) = span (/= ',') line
        in (read n1, read (drop 1 n2))
    in map parseLine . lines $ str

process :: [(Int, Int)] -> Int
process coords = maximum [size | (x1, y1) <- coords, (x2, y2) <- coords, let size = (abs (x1-x2)+1) * (abs (y1-y2)+1)]
