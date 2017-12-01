module Main where

import Data.List
import Data.List.Split

import Lib

int_conv :: [String] -> [Int]
int_conv = fmap read

-- Stolen from SO
rotate :: [a] -> [a]
rotate [] = []
rotate xs = do
    let n = quot (length xs) 2
    zipWith const (drop n (cycle xs)) xs

el_score :: Int -> Int -> Int
el_score a b =
    if a == b then a
    else 0

parse_captcha :: [Int] -> Int
parse_captcha ls = do
    let rot = rotate ls
    foldl (+) 0 $ zipWith el_score ls rot

main :: IO ()
main = do
    file_string <- readFile "input.txt"
    -- Must be an easier way to do this:
    let numbers = int_conv $
                  filter (\el -> el /= "") $
                  splitOn "" file_string

    print $ parse_captcha numbers
    

