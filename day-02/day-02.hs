part1 = process . ranges
part2 = undefined

ranges :: String -> [String]
ranges [] = []
ranges (',' : str) = [] : ranges str
ranges (c : str) = case ranges str of
    [] -> [[c]]
    (head : tail) -> (c : head) : tail

process :: [String] -> Int
process = let
    parse :: String -> (String, String)
    parse str = let
        (fst, snd) = span (/= '-') str
        in (fst, drop 1 snd)
    in sum . map (scanRange . parse)


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

scanRange :: (String, String) -> Int
scanRange (lower, upper) = let
        parse :: String -> (Int, Int)
        parse str = mapTuple read $ splitAt (length str `div` 2) str
        (fstLower, sndLower) = parse lower
        (fstUpper, sndUpper) = parse upper
    in (if fstLower >= sndLower then 1 else 0)
        + (if fstUpper <= sndUpper then 1 else 0)
        + (fstUpper - fstLower - 1)

{- strategy:
    - e.g. 123456 - 126789
    - for the lower end, check if [first half][first half] is still within bounds = 123123 x
    - substract first half of lower end from first half of upper end = 3, subtract 1 = 2   y
    - for the upper end, check if [first half][first half] is still within bounds = 126126 z
    - res = x + y + z = 3
-}
