module Day6() where

import System.IO

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