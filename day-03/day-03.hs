import Data.Char (ord)

part1 = map process . lines

-- process one line
{- strategy:
    - x = max digit excluding last battery
    - y = max digit to the right of the leftmost appearance of x
    - result = 10 * x + y
-}
process :: String -> Int
process str = let
    maxDigit s = (ord . maximum) s - ord '0'
    digit1 = (maxDigit . drop 1 . reverse) str
    in digit1
