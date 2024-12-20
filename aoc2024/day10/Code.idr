module Main

import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Data.SortedMap
import Decidable.Equality
import Util


data Dir = Up | Down | Left | Right

Eq Dir where
    Up == Up = True
    Down == Down = True
    Left == Left = True
    Right == Right = True
    _ == _ = False

Show Dir where
    show Up = "U"
    show Down = "D"
    show Left = "L"
    show Right = "R"


Ord Dir where
    compare a b = compare (show a) (show b)


Point : Type
Point = (Int, Int)


Grid : Type
Grid = SortedMap Point Int

charToDigit : Char -> Int
charToDigit c =
  if c >= '0' && c <= '9' then
    -- ord '0' is the code for '0'
    cast (ord c - ord '0')
  else
    0


loadData : String -> Grid
loadData text = go (unpack text) 0 0 empty
  where
    go : List Char -> Int -> Int -> Grid -> Grid
    go [] r c map = map
    go ('\n' :: cs) r c map = go cs (r + 1) 0 map
    go (x :: xs) r c map = go xs r (c + 1) $ insert (r,c) (charToDigit x) map


findTrailHeads : List (k, Int) -> List k
findTrailHeads [] = []
findTrailHeads ((k, v) :: rest) =
    if v == 0 then k :: findTrailHeads rest else findTrailHeads rest


step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)

dirList : List Dir
dirList = [Up, Down, Left, Right]


walk : Grid -> Int -> Point -> List Point
walk grid level pos =
    case lookup pos grid of
        Nothing => []
        Just l =>
            if level /= l then []
            else if level == 9 then [pos]
            else
                concatMap (\dir => walk grid (level + 1) (step dir pos)) dirList


part1 : Grid -> Point -> Int
part1 grid start = cast $ length $ nub $ walk grid 0 start


part2 : Grid -> Point -> Int
part2 grid start = cast $ length $ walk grid 0 start


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := loadData content
        let heads := findTrailHeads (toList grid)
        printLn $ "Part One result: " ++ show (sum $ part1 grid <$> heads)
        printLn $ "Part Two result: " ++ show (sum $ part2 grid <$> heads)


main : IO ()
main = do
    run "aoc2024/day10/test.txt"
    run "aoc2024/day10/input.txt"
