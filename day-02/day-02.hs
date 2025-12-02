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
        lenLower = length fst
        lenUpper = length snd - 1 -- minus is still included
        -- turns 90,110 into 90,99 and 990,1100 into 1000,1100
        clean (fst, snd)
            | lenLower == lenUpper = (fst, drop 1 snd)
            | even lenLower = (fst, replicate lenLower '9')
            | otherwise = ('1':replicate (lenUpper-1) '0', drop 1 snd)
        in clean (fst, snd)
    in sum . map (scanRange . parse)


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

scanRange :: (String, String) -> Int
scanRange (lower, upper)
    | odd $ length lower = 0
    | head <= tail = concatFactor * ((tail-head+1) * (head+tail)) `div` 2
    | otherwise = 0
    where
        partLength = length lower `div` 2
        parse :: String -> (Int, Int)
        parse str = mapTuple read $ splitAt partLength str
        (fstLower, sndLower) = parse lower
        (fstUpper, sndUpper) = parse upper
        concatFactor = read $ '1' : replicate (partLength - 1) '0' ++ ['1']

        head = fstLower + if fstLower < sndLower then 1 else 0
        tail = fstUpper - if fstUpper > sndUpper then 1 else 0

{- generating invalid ids:
    - for the lower bound, check if it would be in range (store x)
    - for the upper bound, check if it would be in range (store y)
    - if yes to both, xx + (x+1)(x+1) + ... + (y-1)(y-1) + yy is the result
    - otherwise, exclude xx and/or yy

    123 + 124 + 125 = ((125-123+1) * (123+125)) /2
-}

{- strategy:
    - e.g. 123456 - 126789
    - for the lower end, check if [first half][first half] is still within bounds = 123123 x
    - substract first half of lower end from first half of upper end = 3, subtract 1 = 2   y
    - for the upper end, check if [first half][first half] is still within bounds = 126126 z
    - res = x + y + z = 3
-}
