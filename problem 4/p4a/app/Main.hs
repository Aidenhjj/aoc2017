module Main where

import Data.List
import Data.List.Split
import Lib

valid_passphrase :: [String] -> Bool
valid_passphrase [] = error "blank phrase"
valid_passphrase [x] = True
valid_passphrase (x:xs)
    | x `elem` xs = False
    | otherwise = valid_passphrase xs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let passphrases = map (splitOn " ") $ lines content
    let valid_count = length $ filter (\x -> x) $ map valid_passphrase passphrases

    print valid_count
