import Data.List (partition)

part1 = filterIds . parse
part2 = countIds . mergeRanges . sortRanges . fst . parse

parse :: String -> ([(Int, Int)], [Int])
parse str = let
    (rangeStrs, idStrs) = span (/= "") . lines $ str
    parseRange str = let
        (lowerStr, upperStr) = span (/= '-') str
        in (read lowerStr, read . drop 1 $ upperStr)
    ranges = map parseRange rangeStrs
    ids = map read . drop 1 $ idStrs
    in (ranges, ids)

filterIds :: ([(Int, Int)], [Int]) -> Int
filterIds (ranges, ids) = length . filter (isInRange ranges) $ ids

isInRange :: [(Int, Int)] -> Int -> Bool
isInRange [] _ = False
isInRange ((l,u):rs) n = (l <= n && n <= u) || isInRange rs n

-- sort ranges by lower bound
sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges = let
    sort r@(l,_) acc = let
        (head, tail) = partition (\ (l',u') -> l'<l) acc
        in head ++ [r] ++ tail
    in foldr sort []

-- merge overlapping ranges
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ((l1,u1):(l2,u2):r)
    | l2 <= u1 = mergeRanges ((l1,max u1 u2):r)
    | otherwise = (l1,u1) : mergeRanges ((l2,u2):r)
mergeRanges r = r

countIds :: [(Int, Int)] -> Int
countIds = foldr (\ (l, u) acc -> u-l+1 + acc) 0
