part1 = process [] . lines

process :: [Bool] -> [String] -> Int
process b = fst . foldl step (0,b)

step :: (Int, [Bool]) -> String -> (Int, [Bool])
step (0,[]) line = step (0, replicate (length line) False) line
step (acc, beams) line = let
    isPassing isBeam c = c == '.' && isBeam || c == 'S'
    isHitSplitter isBeam c = c == '^' && isBeam
    passingBeams = zipWith isPassing beams line
    splittersHit = zipWith isHitSplitter beams line
    splitBeams = shiftBeams (splittersHit !! 1) splittersHit
    acc' = acc + length (filter id splittersHit)
    in (acc', zipWith (||) passingBeams splitBeams)

shiftBeams :: Bool -> [Bool] -> [Bool]
shiftBeams prev [l,_] = [prev, l]
shiftBeams prev (l:x:r:xs) = prev : shiftBeams (l || r) (x:r:xs)
