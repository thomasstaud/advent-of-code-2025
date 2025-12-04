part1 str = let
    mat = parse str
    initial = stacks mat
    final = stacks $ reduce mat . mapToAdj $ mat
    in initial - final

part2 str = let
    mat = parse str
    initial = stacks mat
    in step 0 initial mat

step :: Int -> Int -> [[Bool]] -> Int
step acc prev mat
    | prev == next = acc
    | otherwise = step (acc+prev-next) next reduced
    where
        reduced = reduce mat . mapToAdj $ mat
        next = stacks reduced

-- paper stack matrix
parse :: String -> [[Bool]]
parse str = [[c == '@' | c <- line] | line <- lines str]

-- maps each coord to the number of adjacent paper stacks
mapToAdj :: [[Bool]] -> [[Int]]
mapToAdj mat = map countAdj $ expandMat mat

expandMat :: [[Bool]] -> [([Bool], [Bool], [Bool])]
expandMat mat = let
    -- expand mat with a row/col of 'False'
    width = length . head $ mat
    extraRow = replicate (width+2) False
    mat' = [extraRow] ++ map (\ row -> [False] ++ row ++ [False]) mat ++ [extraRow]
    in [(t,m,b) | (t, i) <- zip (reverse . drop 2 . reverse $ mat') [0..],
        let m = mat' !! (i+1),
        let b = mat' !! (i+2)]

-- checks how many of the adjacent tiles have paper
countAdj :: ([Bool], [Bool], [Bool]) -> [Int]
countAdj ([t1, t2, t3], [m1, m2, m3], [b1, b2, b3]) =
    [length . filter id $ [t1, t2, t3, m1, m3, b1, b2, b3]]
countAdj (t1:t2:t3:ts, m1:m2:m3:ms, b1:b2:b3:bs) =
    (length . filter id $ [t1, t2, t3, m1, m3, b1, b2, b3])
    : countAdj (t2:t3:ts, m2:m3:ms, b2:b3:bs)

-- removes all accessible stacks
reduce :: [[Bool]] -> [[Int]] -> [[Bool]]
reduce mat adj = let
    inaccessible (isStack, adj) = isStack && adj >= 4
    in [[inaccessible pair | pair <- zip matRow adjRow] | (matRow, adjRow) <- zip mat adj]

-- count stacks in the matrix
stacks :: [[Bool]] -> Int
stacks mat = sum [length . filter id $ row | row <- mat]
