import Data.Char (ord, chr)

part1 = sum . map (process 2) . lines
part2 = sum . map (process 12) . lines

process :: Int -> String -> Int
process n str = let
    xs = map (\ c -> ord c - ord '0') str
    conc = foldr ((:) . (\ x -> chr $ x + ord '0')) []
    in read . conc . digits xs $ n

digits :: [Int] -> Int -> [Int]
digits xs 0 = []
digits xs n = let
    digit = maximum . drop (n-1) . reverse $ xs
    remXs = drop 1 . dropWhile (/= digit) $ xs
    in digit : digits remXs (n-1)

