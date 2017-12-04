module Main where

import Lib
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Coord = Coord {
    x :: Int,
    y :: Int
} deriving (Show, Eq, Ord)

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

updateMap :: Map Coord Int -> Int -> Map Coord Int
updateMap myMap n =
    Map.insert (Coord 1 0) n myMap


get_manhattan :: Coord -> Int
get_manhattan (Coord x y) =
    (abs x) + (abs y)

main :: IO ()
main = do
    let m = (Map.empty)
    let k = updateMap m 1
    print $ map (xy . spiral) [1..200]
    -- print k