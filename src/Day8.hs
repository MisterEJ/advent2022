{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day8() where

import System.IO
import Control.Monad ()
import Data.List.Split

size :: Int
size = 98

solve :: IO ()
solve = do
    handle <- openFile "input8" ReadMode
    content <- hGetContents handle
    print $ solve' (parse content)

solve' :: [[Int]] -> Int
solve' l = do
    let enumeration = [(x,y) | x <-[0..size], y <-[0..size]]
    let fl = check l <$> enumeration
    maximum fl

parse :: String -> [[Int]]
parse s = do
    let spl = chunksOf 1 <$> splitOn "\n" s
    init (map (read <$>) spl)

grab :: [[Int]] -> (Int, Int) -> Int
grab l (x,y) = (l !! y) !! x

sightLine :: [Int] -> Int -> Int
sightLine [] _ = 0
sightLine (x:xs) h
    | x < h = 1 + sightLine xs h
    | x >= h = 1
sightLine _ _ = 0

check :: [[Int]] -> (Int, Int) -> Int
check l (x,y) = do
    let left = reverse $ take x (l !! y)
    let right = drop (x+1) (l !! y)
    let value = grab l (x,y)

    let foundL = sightLine left value
    let foundR = sightLine right value

    let enumeration = [(x,y'::Int) | y' <- [0..size]]
    let column = grab l <$> enumeration

    let foundT = sightLine (reverse $ take y column) value
    let foundB = sightLine (drop (y+1) column) value

    foundL * foundR * foundB * foundT