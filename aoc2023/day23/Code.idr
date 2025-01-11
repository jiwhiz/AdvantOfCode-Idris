module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Control.Monad.State
import Util
import Data.Queue

Point : Type
Point = (Int, Int)


Grid : Type
Grid = SortedMap Point Char


data Dir = Up | Down | Left | Right

Eq Dir where
    Up == Up = True
    Down == Down = True
    Left == Left = True
    Right == Right = True
    _ == _ = False

Show Dir where
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"

Ord Dir where
    compare a b = compare (show a) (show b)

dirList : List Dir
dirList = [Up, Down, Left, Right]


parseGrid : List String -> Grid
parseGrid lines = go 0 lines empty
    where
        parseRow : Int -> Int -> List Char -> Grid -> Grid
        parseRow _ _ [] grid = grid
        parseRow row col (c::cs) grid = parseRow row (col + 1) cs (insert (row, col) c grid)

        go : Int -> List String -> Grid -> Grid
        go _ [] grid = grid
        go row (x::xs) grid = go (row + 1) xs (parseRow row 0 (unpack x) grid)


step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)


hiking : Bool -> Grid -> Point -> Point -> Nat
hiking slippery grid start end =
    let (map, _) = runState empty (process $ Queue.singleton (end, []))
        Just path = lookup start map | Nothing => 0
    in length path
    where
        neighbors : Point -> List Point -> List Point
        neighbors pos path = foldl
            (\acc, dir =>
                let next = step dir pos in
                if contains next path then acc
                else case lookup next grid of
                    Nothing => acc
                    Just '#' => acc
                    _ => next :: acc
            )
            []
            (availableDirs pos)
            where
                contains : Point -> List Point -> Bool
                contains point [] = False
                contains point (p::ps) = if p == point then True else contains point ps
                
                availableDirs : Point -> List Dir
                availableDirs p =
                    if slippery then 
                        case lookup p grid of -- opposite direction
                            Just 'v' => [Up]
                            Just '^' => [Down]
                            Just '>' => [Left]
                            Just '<' => [Right]
                            Just '.' => dirList
                            _ => []
                    else dirList
        
        checkMemo : Queue (Point, List Point) -> Point -> List Point -> State (SortedMap Point (List Point)) (Queue (Point, List Point))

        process : Queue (Point, List Point) -> State (SortedMap Point (List Point)) ()
        process queue =
            do
                memo <- get
                let Just ((curPos, path), queue') := dequeue queue | Nothing => pure ()
                let newPath := curPos :: path
                queue'' <- foldlM
                        (\q, p =>
                            checkMemo q p newPath
                        )
                        queue'
                        (neighbors curPos path)
                process queue''

        checkMemo queue pos newPath = do
            memo <- get
            case lookup pos memo of
                Nothing => do
                    modify (insert pos newPath) 
                    pure (enqueue (pos, newPath) queue)
                Just oldPath =>
                    if length newPath >= length oldPath
                        then do
                            modify (insert pos newPath)
                            pure (enqueue (pos, newPath) queue)
                        else pure queue


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let grid := parseGrid $ lines content
        let Just start := head' $ drop 1 $ keys grid | _ => printLn "No grid data"
        let Just end := head' $ drop 1 $ reverse $ keys grid | _ => printLn "No grid data"
        putStrLn $ "Part One result: " ++ (show $ hiking True  grid start end)
        putStrLn $ "Part Two result: " ++ (show $ hiking False grid start end)


main : IO ()
main = do
    run "aoc2023/day23/test.txt"
    run "aoc2023/day23/input.txt"
