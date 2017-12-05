module Main where

import Lib
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Coord = Coord {
    x :: Int,
    y :: Int
} deriving (Show, Eq, Ord)

add_coord :: Coord -> Coord -> Coord
add_coord a b = (Coord ((x a)+(x b)) ((y a)+(y b)))

data State = State {
    xy :: Coord,
    dir :: Char
} deriving (Show)

spiral_next :: State -> State
spiral_next (State xy dir)
    | ((x xy) == 0) && ((y xy) == 0) = State (Coord 1 0) 'u'
    | ((y xy) > 0) && ((x xy) == (y xy)) && (dir == 'u') = State (Coord ((x xy)-1) (y xy)) 'l'
    | ((x xy) < 0) && ((-(x xy)) == (y xy)) && (dir == 'l') = State (Coord (x xy) ((y xy)-1)) 'd'
    | ((y xy) < 0) && ((x xy) == (y xy)) && (dir == 'd') = State (Coord ((x xy)+1) (y xy)) 'r'
    | ((x xy) > 0) && (((x xy)-1) == (-(y xy))) && (dir == 'r') = State (Coord (x xy) ((y xy)+1)) 'u'
    | dir == 'u' = State (Coord (x xy) ((y xy)+1)) dir
    | dir == 'l' = State (Coord ((x xy)-1) (y xy)) dir
    | dir == 'd' = State (Coord (x xy) ((y xy)-1)) dir
    | dir == 'r' = State (Coord ((x xy)+1) (y xy)) dir
    | otherwise = error "Invalid State"

spiral :: Int -> State
spiral n
    | n < 1 = error "Non-positive index invalid"
    | n == 1 = State (Coord 0 0) 'r'
    | otherwise = spiral_next $ spiral (n-1)

update_map :: Map Coord Int -> Coord -> Map Coord Int
update_map my_map xy
    | xy == (Coord 0 0) = Map.insert xy 1 my_map
    | otherwise = Map.insert xy val my_map
    where val = get_neighbour_sum my_map xy

list_to_coord :: [Int] -> Coord
list_to_coord xy = (Coord (xy !! 0) (xy !! 1))

get_neighbour_sum :: Map Coord Int -> Coord -> Int
get_neighbour_sum my_map xy = do
    let deltas = map list_to_coord $ filter (\x -> x /= [0,0]) $ sequence [[1,0,-1],[1,0,-1]]
    let neighbours = map (add_coord xy) deltas
    let adj_scores = map (\x -> Map.lookup x my_map) neighbours
    foldl (+) 0 $ catMaybes adj_scores

-- get_manhattan :: Coord -> Int
-- get_manhattan (Coord x y) =
--     (abs x) + (abs y)

get_max_score :: Map Coord Int -> Int
get_max_score my_map
    | my_map == Map.empty = 0
    | otherwise = maximum $ Map.elems my_map

-- fold_me :: (Map Coord Int -> Coord -> Map Coord Int) -> Map Coord Int -> [Coord] -> 

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

main :: IO ()
main = do
    let score_map = (Map.empty)
    let coords_inf = map (xy . spiral) [1..]
    -- let coords = map (xy . spiral) [1..100]

    let max_allowed = 265149

    -- let map_out = foldl update_map score_map coords

    let map_out = takeWhileOneMore (\m -> (get_max_score m) < max_allowed) $ scanl update_map score_map coords_inf

    print map_out
    print $ get_max_score $ last map_out

