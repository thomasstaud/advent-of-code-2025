part1 = process 2
part2 = process 12

process :: Int -> String -> Int
process n = sum . map (read . digits n) . lines

digits :: Int -> String -> String
digits 0 xs = []
digits n xs = let
    digit = maximum . drop (n-1) . reverse $ xs
    remXs = drop 1 . dropWhile (/= digit) $ xs
    in digit : digits (n-1) remXs
