module Main where

import System.IO
import Control.Monad ()
import Data.List.Split
import Data.List (sort)

main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    content <- hGetContents handle
    let sums = reverse $ sort $ getInts $ splitOn "\n\n" content
    print $ sum [sums !! 0, sums !! 1, sums !! 2]

getInts :: [String] -> [Int]
getInts = map (read' . words)

read' :: [String] -> Int
read' s = do
    let cont = map read s
    sum cont
