{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day7() where

import System.IO
import Control.Monad ()
import Data.List.Split
import Data.List (elemIndex)

data Value = Size Int | Dir String deriving (Show)
newtype Map = Map [(String, [Value])] deriving (Show)

solve :: IO ()
solve = do
    handle <- openFile "input7" ReadMode
    content <- hGetContents handle
    let sp = tail $ splitOn "\n" <$> splitOn "$ " content
    print $ solve' $ dirInt (Map (parse sp "")) (Map (parse sp ""))

totalSize :: Int
totalSize = 70000000

neededSize :: Int
neededSize = 30000000

solve' :: [Int] -> Int
solve' l = do
    let freespace = totalSize - head l
    let x = (\y -> y-(neededSize - freespace)) <$> l
    (neededSize - freespace) + minimum (filter (>= 0) x)

dirInt :: Map -> Map -> [Int]
dirInt (Map []) _ = []
dirInt (Map (x:xs)) m = getSize (snd x) m : dirInt (Map xs) m

getSize :: [Value] -> Map -> Int
getSize [] _ = 0
getSize ((Size i):xs) m = i + getSize xs m
getSize ((Dir d):xs) m = getSize (getValues d m) m + getSize xs m

getValues :: String -> Map -> [Value]
getValues _ (Map []) = []
getValues key (Map (x:xs)) = if fst x == key then snd x else getValues key (Map xs)

parse :: [[String]] -> String -> [(String, [Value])]
parse [] _ = []
parse ((x:xs):lines') d = do
    if take 2 x == "cd"
        then do
            let dir = head $ tail $ splitOn " " x
            parse lines' (cd d dir)
        else if take 2 x == "ls"
            then do
                (d, convertToValue xs d) : parse lines' d
            else []
parse _ _ = []

cd :: String -> String -> String
cd _ "/" = "/"
cd c ".." = do
    let t = drop (1 + index (elemIndex '/' (reverse c))) (reverse c)
    reverse t
cd "/" n = "/" ++ n
cd c n = c ++ "/" ++ n

index :: Maybe Int -> Int
index Nothing = 0
index (Just i) = i

convertToValue :: [String] -> String -> [Value]
convertToValue [] _ = []
convertToValue [""] _ = []
convertToValue (x:xs) d = do
    let spl = splitOn " " x
    let next = convertToValue xs d
    if head spl == "dir" then Dir (cd d (spl !! 1)) : next
    else Size (read $ head spl) : next

