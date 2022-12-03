module Day3() where

import System.IO
import Control.Monad ()
import Data.List.Split
import Data.List (elemIndex)

solve :: IO ()
solve = do
    handle <- openFile "input3" ReadMode
    content <- hGetContents handle
    let sp = splitOn "\n" content
    print $ sum $ calculateSum $ solve' sp
    --print $ calculateSum $ solve' (splitOn "\n" content)

solve' :: [String] -> String
solve' [] = []
solve' (a:b:c:xs) = do
    if f a (b,c) == '-'
        then solve' xs
        else f a (b,c) : solve' xs
    where
        f :: String -> (String, String) -> Char
        f [] (_,_) = '-'
        f (x':xs') (a, b) = do
            let index1 = getIndex $ elemIndex x' a
            let index2 = getIndex $ elemIndex x' b
            if index1 /= -1 && index2 /= -1
                then a !! index1
                else f xs' (a, b)
solve' _ = []

calculateSum :: String -> [Int]
calculateSum [] = []
calculateSum (x:xs) = do
    let i = fromEnum x
    if i <= 90
        then (i - 64 + 26) : calculateSum xs
        else (i - 96) : calculateSum xs


getIndex :: Maybe Int -> Int
getIndex Nothing = -1
getIndex (Just i) = i