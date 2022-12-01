module Main where

import System.IO
import Control.Monad ()
import Data.List.Split

main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    content <- hGetContents handle
    print $ splitOn "\n" content