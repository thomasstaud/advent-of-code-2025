module Day01r(part1, part2) where

part1 = process score1
part2 = process score2

type Scoring = Int -> Int -> Int -> Int
score1 n' _ _ = if n' == 0 then 1 else 0
score2 n' n x = div (abs x) 100 + if n' == 0 || n /= 0 && signum (n'-n) /= signum x then 1 else 0

-- parse rotations and process via foldl
process :: Scoring -> String -> Int
process score input = snd $ foldl (step score) (50, 0) xs
    where xs = [x | (c : str) <- lines input, let x = read str * if c == 'R' then 1 else -1]

-- n .. current state, n' .. next state, x .. current rotation
type State = (Int, Int)
step :: Scoring -> State -> Int -> State
step score (n, res) x = (n', res')
    where
        n' = mod (n + x) 100
        res' = res + score n' n x
