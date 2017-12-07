module Main where

import Data.List
import Data.Function (on)
import Data.List.Split
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lib

data State = State {
    banks :: [Int],
    pos :: Int,
    to_distribute :: Int,
    cycles :: Int
} deriving (Show, Eq)

int_conv :: [String] -> [Int]
int_conv = map read

-- get_numbers :: String -> [Int]
-- get_numbers num_str = read $ splitOn "\t" num_str

distribute_mem :: State -> State
distribute_mem st = do
    let ls = banks st
    let i = pos st
    State (add_to_elem ls (i, 1)) (get_next_pos (length ls) i) ((to_distribute st)-1) (cycles st)

get_next_pos :: Int -> Int -> Int
get_next_pos len p
    | (p+1) >= (len) = 0
    | otherwise = (p+1)

add_to_elem :: [Int] -> (Int, Int) -> [Int]
add_to_elem ls (i, d) = do
    let (x,el:xs) = splitAt i ls
    (x ++ (el+d) : xs)

get_max_pos :: (Int, Int) -> (Int, Int) -> (Int, Int)
get_max_pos (p_i, prev) (i, val)
    | val > prev = (i, val)
    | otherwise = (p_i, prev)

find_first_max :: [Int] -> (Int, Int)
find_first_max ls = foldl get_max_pos (-1, 0) (zip [0..] ls)

next_state :: State -> State
next_state st
    | (to_distribute st) == 0 = State (add_to_elem (banks st) (m_pos, (-n))) (get_next_pos (length (banks st)) m_pos) n ((cycles st)+1)
    | otherwise = distribute_mem st
    where (m_pos, n) = find_first_max (banks st)

update_map :: Map [Int] Int -> [Int] -> Map [Int] Int
update_map my_map ls = Map.insert ls (Map.size my_map) my_map

find_cycle :: State -> Map [Int] Int -> Int
find_cycle state_in my_map
    | (Map.member ls my_map) = (Map.size my_map)
    | (to_distribute state_in) == 0 = find_cycle (next_state state_in) (update_map my_map ls)
    | otherwise = find_cycle (next_state state_in) my_map
    where ls = banks state_in

main :: IO ()
main = do
    content <- readFile "input.txt"
    -- let memory = int_conv $ lines content

    let memory = int_conv $ splitOn "\t" content
    -- print memory
    -- print $ int_conv $ splitOn "\t" content

    let test = State [0,2,7,0] 0 0 0
    -- mapM_ print $ sortBy (compare `on` snd) $ Map.assocs $ find_cycle test (Map.empty)
    print $ find_cycle test (Map.empty)
    print $ find_cycle (State memory 0 0 0) (Map.empty)


    -- print $ find_cycle (State memory 0 0) (Map.empty)