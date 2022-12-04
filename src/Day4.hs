module Day4() where

import System.IO
import Control.Monad ()
import Data.List.Split

solve :: IO ()
solve = do
    handle <- openFile "input4" ReadMode
    content <- hGetContents handle
    print $ solve' $ parse content

solve' :: [((Int, Int),(Int, Int))] -> Int
solve' [] = 0
solve' (x:xs) = do
    if check x then 1 + solve' xs
    else 0 + solve' xs

check :: ((Int, Int),(Int, Int)) -> Bool
check ((x1,y1), (x2, y2)) = x1 <= y2 && x2 <= y1


parse :: String -> [((Int, Int),(Int, Int))]
parse s = do
    let first = splitOn "\n" s
    parse' first
    where
        parse' :: [String] -> [((Int, Int),(Int, Int))]
        parse' [] = []
        parse' ("":xs) = []
        parse' (x:xs) = do
            let first' = splitOn "," x
            let sec1 = splitOn "-" (first' !! 0)
            let sec2 = splitOn "-" (first' !! 1)
            ((read $ sec1 !! 0, read $ sec1 !! 1),(read $ sec2 !! 0, read $ sec2 !! 1)) : parse' xs
