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
    if el >= 3 then x ++ (el-1) : xs
    else x ++ (el+1) : xs
    

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

    -- let test = State [0,3,0,1,-3] 0
    -- print $ run_maze test 0

    print $ run_maze (State m 0) 0
    -- print maze

    -- let m2 = next_state test
    -- let m3 = next_state m2
    -- let m4 = next_state m3
    -- let m5 = next_state m4
    -- let m6 = next_state m5
    -- let m7 = next_state m6
    -- let m8 = next_state m7
    -- let m9 = next_state m8
    -- let m10 = next_state m9
    -- let m11 = next_state m10

    -- print test
    -- print m2
    -- print m3
    -- print m4
    -- print m5
    -- print m6
    -- print m7
    -- print m8
    -- print m9
    -- print m10
    -- print m11

    -- print maze
    -- print "----"
    -- print $ increment_elem maze 2
