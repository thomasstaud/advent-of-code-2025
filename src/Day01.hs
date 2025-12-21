module Day01(main) where

main = do
    file <- readFile "inputs/01.txt"
    let input = parse file
    print $ part1 input
    print $ part2 input

part1 = process score1
part2 = process score2

parse file = [x | (c : str) <- lines file, let x = read str * if c == 'R' then 1 else -1]

type Scoring = Int -> Int -> Int -> Int
score1 n' _ _ = if n' == 0 then 1 else 0
score2 n' n x = div (abs x) 100 + if n' == 0 || n /= 0 && signum (n'-n) /= signum x then 1 else 0

-- parse rotations and process via foldl
process :: Scoring -> [Int] -> Int
process score xs = snd $ foldl (step score) (50, 0) xs

-- n .. current state, n' .. next state, x .. current rotation
type State = (Int, Int)
step :: Scoring -> State -> Int -> State
step score (n, res) x = (n', res')
    where
        n' = mod (n + x) 100
        res' = res + score n' n x
