import Data.Char (ord, chr)

part1 = sum . map process . lines

process :: String -> Int
process str = let
    xs = map (\ c -> ord c - ord '0') str
    -- apply function f on xs, then find maximum excluding first index
    findMax f = maximum . drop 1 . f $ xs
    digit1 = findMax reverse
    digit2 = findMax $ dropWhile (/= digit1)
    in 10 * digit1 + digit2

