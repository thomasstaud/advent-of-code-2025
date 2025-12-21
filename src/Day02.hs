module Day02(main) where

main = do
    file <- readFile "inputs/02.txt"
    let input = parse file
    print $ part1 input
    print $ part2 input

part1 = process patterns1
part2 = process patterns2

patterns1 str = [length str `div` 2]
patterns2 str = [1..length str `div` 2]

parse :: String -> [(String, String)]
parse = let
    splitComma :: Char -> [String] -> [String]
    splitComma ',' xs = [] : xs
    splitComma c [] = [[c]]
    splitComma c (x:xs) = (c:x) : xs
    cleanRanges :: [String] -> [(String, String)]
    cleanRanges [] = []
    cleanRanges (str : tail) = let
        (fst, snd) = span (/= '-') str
        lenLower = length fst
        lenUpper = length snd - 1 -- minus is still included
        clean (fst, snd)
            | lenLower == lenUpper = [(fst, drop 1 snd)]
            | otherwise = [(fst, replicate lenLower '9'), ('1':replicate (lenUpper-1) '0', drop 1 snd)]
        in clean (fst, snd) ++ cleanRanges tail
    in cleanRanges . foldr splitComma []



process :: (String -> [Int]) -> [(String, String)] -> Int
process patterns = let
    expand :: [(String, String)] -> [String]
    expand [] = []
    expand ((lower, upper) : tail) = map show [read lower..read upper :: Int] ++ expand tail
    in sum . map read . filter (isInvalid patterns) . expand

isInvalid :: (String -> [Int]) -> String -> Bool
isInvalid patterns str = or [invalid | step <- patterns str, step /= 0, let invalid = check str step]
    where
        len = length str
        check :: String -> Int -> Bool
        check str step
            | len `mod` step /= 0 = False
            | otherwise = concat (replicate (len `div` step) (take step str)) == str
