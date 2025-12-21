module Main where

import Data.Time
import Control.Monad
import System.Environment (getArgs)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12

days :: [IO ()]
days = [return (), Day01.main, Day02.main, Day03.main, Day04.main, Day05.main, Day06.main,
  Day07.main, Day08.main, Day09.main, Day10.main, Day11.main, Day12.main]

main :: IO ()
main = do
  args <- getArgs
  if null args then runAll else run (read $ head args)

run :: Int -> IO ()
run day = do
  startTime <- getCurrentTime

  putStrLn ("\n[ Day " ++ show day ++ " ]")
  days !! day

  stopTime <- getCurrentTime
  print $ diffUTCTime stopTime startTime

runAll :: IO ()
runAll = do
    startTime <- getCurrentTime

    forM_ [1..final] run

    stopTime <- getCurrentTime
    putStrLn "\ntotal:"
    print $ diffUTCTime stopTime startTime
  where
    final = length days - 1
