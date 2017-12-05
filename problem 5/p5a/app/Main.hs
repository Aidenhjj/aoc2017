module Main where

import Data.List
import Lib

data State = State {
    maze :: [Int],
    pos :: Int
} deriving (Show, Eq)

int_conv :: [String] -> [Int]
int_conv = fmap read

increment_elem :: State -> [Int]
increment_elem st = do
    let ls = maze st
    let i = pos st
    let (x,el:xs) = splitAt i ls
    x ++ (el+1) : xs

next_state :: State -> State
next_state st = State (increment_elem st) ((pos st)+((maze st) !! (pos st)))

run_maze :: State -> Int -> Int
run_maze state_in steps
    | ((pos state_in) >= (length $ maze state_in)) || ((pos state_in) < 0) = steps
    | otherwise = run_maze (next_state state_in) (steps+1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let m = int_conv $ lines content
    
    print $ run_maze (State m 0) 0
