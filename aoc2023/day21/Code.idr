module Main

import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Data.SortedMap
import Data.SortedSet
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
Grid = SortedMap Point Char


loadData : String -> Grid
loadData text = go (unpack text) 0 0 empty
  where
    go : List Char -> Int -> Int -> Grid -> Grid
    go [] r c map = map
    go ('\n' :: cs) r c map = go cs (r + 1) 0 map
    go (x :: xs) r c map = go xs r (c + 1) $ insert (r,c) x map


findStart : Char -> List (k, Char) -> Maybe k
findStart _ [] = Nothing
findStart symbol ((k, v) :: rest) =
    if v == symbol then Just k else findStart symbol rest


step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)


dirList : List Dir
dirList = [Up, Down, Left, Right]


walk : Grid -> List Point -> List Point
walk grid list =
    SortedSet.toList $ go list SortedSet.empty
    where
        go : List Point -> SortedSet Point -> SortedSet Point
        go [] set = set
        go (p::ps) set =
            go ps $ foldl 
                (\s, dir =>
                    let next = step dir p
                    in case lookup next grid of
                        Just 'S' => insert next s
                        Just '.' => insert next s
                        _ => s
                )
                set
                dirList


repeatN : Nat -> (a -> a) -> a -> a
repeatN Z f x = x
repeatN (S n) f x = repeatN n f (f x)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := loadData content
        let Just start := findStart 'S' (toList grid) | Nothing => printLn "Cannot find start point"
        printLn $ "Part One result: " ++ show (length $ repeatN 64 (walk grid) [start])


main : IO ()
main = do
    run "aoc2023/day21/test.txt"
    run "aoc2023/day21/input.txt"
