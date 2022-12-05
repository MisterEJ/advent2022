module Day5() where

import System.IO
import Control.Monad (when)
import Data.List.Split
import Data.Monoid

solve :: IO ()
solve = do
    handle <- openFile "input5" ReadMode
    content <- hGetContents handle
    let parsed = parse content
    print $ uncurry solve' parsed

data Instruction = Instruction Int Int Int deriving (Show)

solve' :: [String] -> [Instruction]  -> [String]
solve' l i = foldl solve'' l i

solve'' :: [String] -> Instruction -> [String]
solve'' l (Instruction c f t) = do
    let given = move (l !! f) (l !! t) c
    let taken = drop c (l !! f)
    [if x == f then taken else if x == t then given else l !! x | x<-[0..8]]

move :: String -> String -> Int -> String
move s1 s2 i = (take i s1) ++ s2

parse:: String -> ([String], [Instruction])
parse s = do
    let temp = splitOn "\n" s
    let first = take 8 temp
    let second = drop 10 temp
    (clean <$> parseStack first, parseIns second)
    where
        parseStack :: [String] -> [String]
        parseStack [] = replicate 9 "         "
        parseStack (x:xs) = do
            let temp = splitEvery 4 x
            let temp2 = map (!! 1) temp
            let next = parseStack xs
            [(temp2 !! x') : (next !! x') | x' <- [0..8]]

        parseIns :: [String] -> [Instruction]
        parseIns [] = []
        parseIns ("":_) = []
        parseIns (x:xs) = do
            let spl = splitOn " " x
            Instruction (read $ spl !! 1) (read (spl !! 3)-1) (read (spl !! 5)-1) : parseIns xs

--parseIns :: String -> [Instruction]

clean :: String -> String
clean [] = []
clean (x:xs) = if x == ' ' then clean xs else x : clean xs

push :: Char -> [Char] -> [Char]
push x xs = x : xs

pop :: [Char] -> (Char, [Char])
pop [] = ('-', [])
pop (x:xs) = (x, xs)