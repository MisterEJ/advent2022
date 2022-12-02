module Day2(solve) where

import System.IO
import Control.Monad ()
import Data.List.Split
import Control.Concurrent (yield)

solve :: IO ()
solve = do
    handle <- openFile "input2" ReadMode
    content <- hGetContents handle
    print $ solve' $ map predict (splitOn "\n" content)

solve' :: [String] -> Int
solve' [] = 0
solve' ("":xs) = solve' xs
solve' (x:xs) = play x + solve' xs

predict :: String -> String
predict "" = ""
predict s = do
    if last s == 'Z'
        then do
            let x = fromEnum (head s)
            let pred' = if x + 1 <= 67
                then x + 1
                else x - 2
            [head s, ' ', toEnum pred']
        else if last s == 'Y'
            then [head s, ' ', head s]
            else do
                let x = fromEnum (head s)
                let pred' = if x - 1 >= 65
                    then x - 1
                    else x + 2
                [head s, ' ', toEnum pred']


play :: String -> Int
play s = do
    let x = fromEnum (head s) - 64
    let y = fromEnum (last s) - 64
    let res = y - x
    let sum = y + do
        if res == 0
            then 3
            else if res == 1 || res == -2
                then 6
                else 0
    sum
