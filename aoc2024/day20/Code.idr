module Main

import Data.List
import Data.SortedMap
import Data.String
import Util

Point : Type
Point = (Int, Int)


Grid : Type
Grid = SortedMap Point Char


parseGrid : List String -> Grid
parseGrid lines = go 0 lines empty
    where
        parseRow : Int -> Int -> List Char -> Grid -> Grid
        parseRow _ _ [] grid = grid
        parseRow row col (c::cs) grid = parseRow row (col + 1) cs (insert (row, col) c grid)

        go : Int -> List String -> Grid -> Grid
        go _ [] grid = grid
        go row (x::xs) grid = go (row + 1) xs (parseRow row 0 (unpack x) grid)


find : Char -> Grid -> Maybe Point
find c grid =
    head' $ map fst $ filter ((== c) . snd ) $ SortedMap.toList grid


distant : Point -> Point -> Int
distant (row1, col1) (row2, col2) = abs (row1 - row2) + abs (col1 - col2)


getPath : Point -> List Point -> List Point
getPath start [] = [start]
getPath start rest =
    case filter (\x=> distant start x == 1) rest of
        [next] => start :: getPath next (filter ((/=) next) rest)
        _ => [] -- something wrong!


countCheat : Nat -> Int -> List Point -> Int
countCheat minSave steps [] = 0
countCheat minSave steps (x::xs) =
    (Builtin.fst $ foldl 
        (\(acc, idx), p => if distant p x <= steps && distant p x <= idx then (acc + 1, idx+1) else (acc, idx+1))
        (0, 0)
        (getAhead minSave (x::xs))
    )
    + countCheat minSave steps xs
    where
        getAhead : Nat -> List Point -> List Point
        getAhead _ [] = []
        getAhead Z rest = rest
        getAhead (S k) (p::ps) = getAhead k ps



run : String -> Nat -> IO ()
run filename minSave =
    do
        printLn filename
        content <- Util.loadFile filename
        let grid := parseGrid $ lines content

        let Just start := find 'S' grid | _ => printLn "No start point"
        let path := getPath start $ map (\(p, c) => p) (filter (\(_, c) => c == '.' || c == 'E' ) $ SortedMap.toList grid)

        printLn $ "Part One result: " ++ (show $ countCheat minSave 2 path)
        printLn $ "Part Two result: " ++ (show $ countCheat minSave 20 path)



main : IO ()
main = do
    run "aoc2024/day20/test.txt" 50
    run "aoc2024/day20/input.txt" 100
