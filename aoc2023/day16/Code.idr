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
Grid = SortedMap Point (Char, Vect 4 Bool)


loadData : String -> Grid
loadData text = go (unpack text) 0 0 empty
    where
        go : List Char -> Int -> Int -> Grid -> Grid
        go [] r c map = map
        go ('\n' :: cs) r c map = go cs (r + 1) 0 map
        go (x :: xs) r c map = go xs r (c + 1) $ insert (r,c) (x,[False, False, False, False]) map


process : Grid -> Point -> Dir -> Grid 
process grid position dir =
    case lookup position grid of
        Nothing => grid
        Just (char, beams) =>
            if enteredBefore beams dir then grid
            else foldl
                (\g, (d, p) => process g p d
                )
                (insert position (char, energize beams dir) grid)
                (nextSteps char dir position)
    where
        enteredBefore : Vect 4 Bool -> Dir -> Bool
        enteredBefore [u, d, l, r] = \case {Up => u; Down => d; Left => l; Right => r}

        energize : Vect 4 Bool -> Dir -> Vect 4 Bool
        energize [u, d, l, r] = \case {Up => [True, d, l, r]; Down => [u, True, l, r]; Left => [u, d, True, r]; Right => [u, d, l, True]}

        next : Dir -> Point -> (Dir, Point)
        next Up (row, col) = (Up, (row-1, col))
        next Down (row, col) = (Down, (row+1, col))
        next Left (row, col) = (Left, (row, col-1))
        next Right (row, col) = (Right, (row, col+1))

        forwardMirror : Dir -> Dir
        forwardMirror = \case {Up=>Right; Down=>Left; Left=>Down; Right=>Up}

        backwardMirror : Dir -> Dir
        backwardMirror = \case {Up=>Left; Down=>Right; Left=>Up; Right=>Down}

        vertSplitter1 : Dir -> Dir
        vertSplitter1 = \case {Up=>Up; Down=>Down; Left=>Up; Right=>Up}

        vertSplitter2 : Dir -> Dir
        vertSplitter2 = \case {Up=>Up; Down=>Down; Left=>Down; Right=>Down}

        horizSplitter1 : Dir -> Dir
        horizSplitter1 = \case {Up=>Right; Down=>Right; Left=>Left; Right=>Right}

        horizSplitter2 : Dir -> Dir
        horizSplitter2 = \case {Up=>Left; Down=>Left; Left=>Left; Right=>Right}

        nextSteps : Char -> Dir -> Point -> List (Dir, Point)
        nextSteps '.' dir pos = [next dir pos]
        nextSteps '/' dir pos = [next (forwardMirror dir) pos]
        nextSteps '\\' dir pos = [next (backwardMirror dir) pos]
        nextSteps '|' dir pos = [next (vertSplitter1 dir) pos, next (vertSplitter2 dir) pos]
        nextSteps '-' dir pos = [next (horizSplitter1 dir) pos, next (horizSplitter2 dir) pos]
        nextSteps _ _ _ = []


count : Grid -> Nat
count = length . filter (\(c, [u, d, l, r]) => u || d || r || l) . Data.SortedMap.values


part1 : Grid -> Nat
part1 grid = count $ process grid (0,0) Right


part2 : Nat -> Grid -> Nat
part2 Z _ = Z
part2 (S k) grid =
    let left   = map (\n => count $ process grid (n, 0) Right)     $ iterateN (S k) (+ 1) 0
        right  = map (\n => count $ process grid (n, cast k) Left) $ iterateN (S k) (+ 1) 0
        top    = map (\n => count $ process grid (0, n) Down)      $ iterateN (S k) (+ 1) 0
        bottom = map (\n => count $ process grid (cast k, n) Up)   $ iterateN (S k) (+ 1) 0
    in foldl max 0 (left ++ right ++ top ++ bottom)


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let grid := loadData content
        (l::ls) := lines content | _ => printLn "No data"
    putStrLn $ "Part One result: " ++ (show $ part1 grid)
    putStrLn $ "Part Two result: " ++ (show $ part2 (S $ length ls) grid)


main : IO ()
main = do
    run "aoc2023/day16/test.txt"
    run "aoc2023/day16/input.txt"
