{-
https://www.hackerrank.com/challenges/compare-the-triplets/problem
-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module HR_CompareTheTriplets
    (compareTheTriplets)
    where

import Control.Monad
import Data.List
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

bob :: Ord a => [a] -> [a] -> Int
bob [] [] = 0
bob (a:alices) (b:bobs)
    | b > a = 1 + (bob alices bobs)
    | otherwise = bob alices bobs

alice :: Ord a => [a] -> [a] -> Int
alice [] [] = 0
alice (a:alices) (b:bobs)
    | a > b = 1 + (alice alices bobs)
    | otherwise = alice alices bobs


-- Complete the solve function below.
solve a b = [alice a b, bob a b]

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

compareTheTriplets :: IO()
compareTheTriplets = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . words $ aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . words $ bTemp

    let result = solve a b

    hPutStrLn fptr $ intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr


