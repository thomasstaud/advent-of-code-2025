module Main where

import qualified Day01 ()
import qualified Day10

main :: IO ()
main = do
  putStrLn "[ Day 10 ]"
  input <- readFile "inputs/10.txt"
  print $ Day10.part1 input
  print $ Day10.part2 input
