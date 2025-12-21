module Day12(main) where

main = do
    file <- readFile "inputs/12.txt"
    let input = parse file
    print $ part1 input

part1 = process

type Shape = [[Bool]]
-- width height counts
data Region = Region Int Int [Int] deriving Show

parse :: String -> ([Shape], [Region])
parse str = let
    -- split (lines str) into blocks at every blank line
    splitBlocks ls = case span (/= "") ls of
        (block, "":tail) -> block : splitBlocks tail
        (block, []) -> [block]
    blocks = splitBlocks (lines str)
    (shapeBlocks, [regionBlock]) = splitAt (length blocks - 1) blocks

    toShape :: [[Char]] -> Shape
    toShape (_:mat) = map (map (== '#')) mat
    toRegion :: String -> Region
    toRegion line = let
        ([sizeStr], countStrs) = splitAt 1 (words line)
        (widthStr, 'x':heightStr) = span (/= 'x') sizeStr
        width = read widthStr
        height = read (takeWhile (/= ':') heightStr)
        counts = map read countStrs
        in Region width height counts

    shapes = map toShape shapeBlocks
    regions = map toRegion regionBlock
    in (shapes, regions)

process :: ([Shape], [Region]) -> Int
process (shapes, regions) = length $ filter (canFit shapes) regions

-- total regions: 1000
--  trivial eliminations: total shape size > total region size
--  removed: 575 / remaining: 425
canFit :: [Shape] -> Region -> Bool
canFit shapes (Region width height counts) = (width * height) >= sum (zipWith (\ c s -> c * shapeSize s) counts shapes)

shapeSize :: Shape -> Int
shapeSize = sum . map (foldr (\ b e -> e + if b then 1 else 0) 0)
