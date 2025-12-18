part1 = process step1
part2 = process step2

process step input = snd $ foldl step (50, 0) rots
    where
        rots = [n | (c : str) <- lines input, let n = if c == 'R' then read str else - read str]

step1 = step (\ state state' rot -> if state' == 0 then 1 else 0)
step2 = step (\ state state' rot -> div (abs rot) 100 + if state /= 0 && (signum (state - state') == signum rot || state' == 0) then 1 else 0)

step updateRes (state, res) rot = (state', res')
    where
        state' = mod (state + rot) 100
        res' = res + updateRes state state' rot
