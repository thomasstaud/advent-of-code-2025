module Main where

import Data.Time
import Control.Monad
import System.Environment (getArgs)

import qualified Day01
import qualified Day10

days :: [IO ()]
days = [return (), Day01.main, Day10.main]

main :: IO ()
main = do
  args <- getArgs
  print args
  
  if null args then runAll else run (read $ head args)

run :: Int -> IO ()
run day = do
  startTime <- getCurrentTime
  putStrLn ("\n[ Day " ++ show day ++ " ]")
  days !! day
  stopTime <- getCurrentTime
  print $ diffUTCTime stopTime startTime

runAll :: IO ()
runAll = let
    final = length days - 1
    helper i = do
      run i
      when (i < final) $ helper (i+1)
  in do
    startTime <- getCurrentTime

    helper 1

    stopTime <- getCurrentTime
    putStrLn "\ntotal:"
    print $ diffUTCTime stopTime startTime
