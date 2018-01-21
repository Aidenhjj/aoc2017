module Main where

import Lib
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Node = Node {
    weight :: Int,
    parent :: String,
    children :: [String]
} deriving (Show, Eq)

add_parent :: Node -> String -> Node
add_parent node parent_name = node {parent = parent_name}

int_conv :: String -> Int
int_conv = read

strip_chars :: String -> [Char] -> String
strip_chars string_in excl =
    filter (not . (`elem` excl)) string_in

update_map :: Map String Node -> String -> Map String Node
update_map my_map string_in = Map.insert nm rep my_map
    where (nm, rep) = get_report string_in


get_report :: String -> (String, Node)
get_report string_in
    | (length cmds) == 2 = ((cmds !! 0), Node w "" [])
    | otherwise = ((cmds !! 0), Node w "" childs)
    where cmds = words string_in
          childs = map (flip strip_chars ",") (drop 3 cmds)
          w = int_conv $ strip_chars (cmds !! 1) "()"

build_tree :: Map String Node -> Int -> Map String Node
build_tree my_map pos
    | Map.size my_map == (pos + 1) = next_map
    | otherwise = build_tree next_map (pos + 1)
    where next_key = (Map.keys my_map) !! pos
          children_list = children (my_map Map.! next_key)
          next_map = take_node_and_update_children my_map next_key children_list


-- iterate through list of children and add in parent
take_node_and_update_children :: Map String Node -> String -> [String] -> Map String Node
take_node_and_update_children my_map parent_name children_names
    | length children_names == 0 = my_map
    | otherwise = take_node_and_update_children new_map parent_name (tail children_names)
    where new_map = Map.adjust (\node -> node {parent = parent_name}) (head children_names) my_map

get_weight :: Map String Node -> String -> Int
get_weight my_tower node_name
    | child_list == [] = weight this_node
    | otherwise = weight this_node + (foldl (+) 0 (map (get_weight my_tower) child_list))
    where child_list = children this_node
          this_node = my_tower Map.! node_name

find_inbalance :: Map String Node -> String -> Int -> Int
find_inbalance my_map name prev_desr
    | isNothing odd_pos = prev_desr
    | otherwise = find_inbalance my_map (child_list !! (fromJust odd_pos)) desired_weight
    where child_list = children (my_map Map.! name)
          child_weights = map (get_weight my_map) child_list
          odd_pos = find_odd child_weights
          odd_child_w = child_weights !! (fromJust odd_pos)
          odd_weight = weight $ my_map Map.! (child_list !! (fromJust odd_pos))
          other_weight = child_weights !! (((fromJust odd_pos) + 1) `mod` (length child_list))
          desired_weight = (other_weight - odd_child_w) + odd_weight

find_odd :: Eq a => [a] -> Maybe Int
find_odd l
    | length l <= 1 = Nothing
    | otherwise = elemIndex 1 $ map (count_occurance l) l

count_occurance :: Eq a => [a] -> a -> Int
count_occurance [] _ = 0
count_occurance xs x = (length . filter (== x)) xs

main :: IO ()
main = do
    content <- readFile "input.txt"

    let reports = lines content
    let reports_map = foldl update_map Map.empty reports

    let tower = build_tree reports_map 0

    let bottom = head $ Map.keys $ Map.filter (\x -> (parent x) == "") tower

    print $ find_inbalance tower bottom 0
