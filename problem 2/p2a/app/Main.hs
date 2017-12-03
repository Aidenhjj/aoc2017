module Main where

import Data.List
import Data.List.Split
import Lib

comp_int :: (Int -> Int -> Bool) -> Int -> Int -> Int
comp_int f a b =
    if f a b then a
    else b

get_numbers :: [String] -> [[Int]]
get_numbers xs = [fmap read $ splitOn "\t" el | el <- xs]

get_line_sum :: [Int] -> Int
get_line_sum ls =
    (foldl (comp_int (>)) (head ls) ls) - (foldl (comp_int (<)) (head ls) ls)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let raw_lines = lines content
    let split_lines = get_numbers raw_lines

    print $ foldl (\x y -> x + (get_line_sum y)) 0 split_lines
