part1 = process . parse parseHorizontal
part2 = process . parse parseVertical


parseHorizontal :: [String] -> [[Int]]
parseHorizontal = transpose . map (map read . words)

transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = [[]]
transpose rows = let
    col = map head rows
    moreRows = map (drop 1) rows
    in col : transpose moreRows

parseVertical :: [String] -> [[Int]]
parseVertical ("":_) = [[]]
parseVertical numStrs = let
    col = reverse $ map head numStrs
    tail = parseVertical $ map (drop 1) numStrs
    in if all (== ' ') col
        then [] : tail
        else case tail of
            x:xs -> (read col : x) : xs

type Operator = (Int -> Int -> Int, Int)
parse :: ([String] -> [[Int]]) -> String -> ([[Int]], [Operator])
parse numParser str = let
    ([opStrs], numStrs) = splitAt 1 . reverse $ lines str
    mapOp "+" = ((+), 0)
    mapOp "*" = ((*), 1)
    ops = map mapOp . words $ opStrs
    nums = numParser numStrs
    in (nums, ops)


process :: ([[Int]], [Operator]) -> Int
process (_, []) = 0
process (nums:moreNums, ops) = let
    ([op], moreOps) = splitAt 1 ops
    in uncurry foldr op nums + process (moreNums, moreOps)
