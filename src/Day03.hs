module Day03(main) where

main = do
    file <- readFile "inputs/03.txt"
    let input = parse file
    print $ part1 input
    print $ part2 input

part1 = process 2
part2 = process 12

parse = lines

process :: Int -> [String] -> Int
process n = sum . map (read . digits n)

digits :: Int -> String -> String
digits 0 _ = []
digits n xs = let
    digit = maximum . drop (n-1) . reverse $ xs
    remXs = drop 1 . dropWhile (/= digit) $ xs
    in digit : digits (n-1) remXs
