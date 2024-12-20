module Main

import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Data.SortedMap
import Decidable.Equality
import Util


-- Define Board and Cell data types

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


turnRight : Dir -> Dir
turnRight Up = Right
turnRight Down = Left
turnRight Left = Up
turnRight Right = Down


walk : Dir -> Point -> Grid -> Grid
walk dir pos grid =
    let 
        grid' = insert pos 'X' grid

        pos' = step dir pos
    in case lookup pos' grid of
        Just '#' => walk (turnRight dir) pos grid'
        Nothing => grid'
        _ => walk dir pos' grid'


Done : Type
Done = SortedMap (Point, Dir) Unit


checkLoop : Grid -> Done -> Dir -> Point -> Bool
checkLoop grid done dir pos =
    let
        Nothing := lookup (pos, dir) done | _ => True

        done' = insert (pos, dir) MkUnit done

        pos' = step dir pos
    in case lookup pos' grid of
        Nothing => False
        Just '#' => checkLoop grid done' (turnRight dir) pos
        Just _ => checkLoop grid done' dir pos'


part2 : Dir -> Point -> Grid -> Done -> Int
part2 dir pos grid done =
    let
        done' = insert (pos, dir) MkUnit done
        grid' = insert pos 'X' grid
        turnDir = turnRight dir
        turnPos = step turnDir pos
        pos' = step dir pos 
    in case lookup pos' grid' of
        Nothing => 0
        Just '#' => part2 turnDir pos grid' done'
        Just 'X' => part2 dir pos' grid' done'
        Just '.' =>
            if checkLoop (insert pos' '#' grid') done' turnDir pos then
                1 + part2 dir pos' grid' done'
            else
                part2 dir pos' grid' done'
        Just _ => part2 dir pos' grid' done'


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := loadData content
        let Just pos := findStart '^' (toList grid) | Nothing => printLn "Cannot find start point"
        let grid' := walk Up pos grid

        printLn $ "Part One result: " ++ show (length $ filter (\c => snd c == 'X') $ Data.SortedMap.toList grid')
        printLn $ "Part Two result: " ++ show (part2 Up pos grid empty)


main : IO ()
main = do
    run "aoc2024/day06/test.txt"
    run "aoc2024/day06/input.txt"

