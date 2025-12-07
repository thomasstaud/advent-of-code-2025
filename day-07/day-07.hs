part1 = fst . process [] . lines
part2 = sum . snd . process [] . lines

process :: [Int] -> [String] -> (Int, [Int])
process b = foldl step (0,b)

step :: (Int, [Int]) -> String -> (Int, [Int])
step (0,[]) line = step (0, replicate (length line) 0) line
step (acc, beams) line = let
    numPassing _ 'S' = 1
    numPassing numBeams c = if c == '.' then numBeams else 0
    numSplit numBeams c = if c == '^' then numBeams else 0
    passingBeams = zipWith numPassing beams line
    splittersHit = zipWith numSplit beams line
    splitBeams = shiftBeams (splittersHit !! 1) splittersHit
    acc' = acc + length (filter (/= 0) splittersHit)
    in (acc', zipWith (+) passingBeams splitBeams)

shiftBeams :: Int -> [Int] -> [Int]
shiftBeams prev [l,_] = [prev, l]
shiftBeams prev (l:x:r:xs) = prev : shiftBeams (l + r) (x:r:xs)
