part1 = process . parse1
part2 = process . parse2

type Operator = (Int -> Int -> Int, Int)

parse1 :: String -> ([[Int]], [Operator])
parse1 str = let
    ([opStrs], numStrs) = splitAt 1 . reverse $ (map words . lines $ str)
    mapOp "+" = ((+), 0)
    mapOp "*" = ((*), 1)
    ops = map mapOp opStrs
    nums = parseHorizontal (map (map read) numStrs)
    in (nums, ops)

parse2 :: String -> ([[Int]], [Operator])
parse2 str = let
    ([opStrs], numStrs) = splitAt 1 . reverse $ lines str
    mapOp "+" = ((+), 0)
    mapOp "*" = ((*), 1)
    ops = map mapOp . words $ opStrs
    nums = parseVertical numStrs
    in (nums, ops)

parseHorizontal :: [[Int]] -> [[Int]]
parseHorizontal ([]:_) = [[]]
parseHorizontal nums = let
    res = map head nums
    moreNums = map (drop 1) nums
    in res : parseHorizontal moreNums

parseVertical :: [String] -> [[Int]]
parseVertical ("":_) = [[]]
parseVertical numStrs = let
    str = reverse $ map head numStrs
    tail = parseVertical $ map (drop 1) numStrs
    in if all (== ' ') str
        then [] : tail
        else case tail of
            x:xs -> (read str : x) : xs


process :: ([[Int]], [Operator]) -> Int
process (_, []) = 0
process (nums:moreNums, ops) = let
    ([op], moreOps) = splitAt 1 ops
    in uncurry foldr op nums + process (moreNums, moreOps)
