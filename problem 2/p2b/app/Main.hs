module Main where

import Data.List
import Data.List.Split
import Lib

comp_int :: (Int -> Int -> Bool) -> Int -> Int -> Int
comp_int f a b =
    if f a b then a
    else b

get_numbers :: [String] -> [[Int]]
get_numbers xs = [fmap read $ words el | el <- xs]

get_divided :: [Int] -> Int
get_divided ls =
    fst $ head $ filter (\x -> (snd x) == 0) $ fmap (\x -> divMod (head x) (last x)) (filter (\x -> (head x) /= (last x)) (sequence [ls,ls]))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let raw_lines = lines content
    let split_lines = get_numbers raw_lines

    print $ get_divided [5,9,2,8]
    print $ get_divided [9,4,7,3]
    print $ get_divided [3,8,6,5]
    print $ foldl (\x y -> x + (get_divided y)) 0 split_lines
    -- print split_lines

