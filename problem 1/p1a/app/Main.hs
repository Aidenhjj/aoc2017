module Main where

import Data.List
import Data.List.Split
-- import Data.Typeable

import Lib

int_conv :: [String] -> [Int]
int_conv = fmap read

el_score :: Int -> Int -> Int
el_score a b =
    if a == b then a
    else 0

parse_captcha :: [Int] -> Int
parse_captcha ls = do
    let new_list = ls ++ [head ls]
    foldl (+) 0 $ zipWith el_score (tail new_list) new_list

main :: IO ()
main = do
    file_string <- readFile "input.txt"
    -- Must be an easier way to do this:
    let numbers = int_conv $
                  filter (\el -> el /= "") $
                  splitOn "" file_string

    print $ parse_captcha numbers
    

