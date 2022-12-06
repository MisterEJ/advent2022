module Day6() where

import System.IO
import Control.Monad (when)
import Data.List.Split
import Data.Monoid

solve :: IO ()
solve = do
    handle <- openFile "input6" ReadMode
    content <- hGetContents handle
    print $ solve' content + 14

solve' :: String -> Int
solve' [] = 0
solve' (x:xs) = if check (take 14 (x:xs)) then 0 else solve' xs + 1


check :: String -> Bool
check [] = True
check [_] = True
check (x:xs) = x `notElem` xs && check xs


check' :: String -> Char -> Int
check' [] _ = 0
check' (x:xs) c = (if x == c then 1 else 0) + check' xs c