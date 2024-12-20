module Main

import Data.List1
import Data.String
import Util


data Problem : Type where
    P : Int -> String -> Problem


getChar : Problem -> Int -> Int -> Char 
getChar (P size text) row col =
    if (row < 0 || row >= size) then '.'
    else if (col < 0 || col >= size) then '.'
    else assert_total $ strIndex text (row * size + col)


check1 : Problem -> Int -> Int -> (Int, Int) -> Int 
check1 prob row col (dr, dc) =
    if (getChar prob row col) /= 'X' then 0
    else if (getChar prob (row + dr) (col + dc)) /= 'M' then 0
    else if (getChar prob (row + dr * 2) (col + dc * 2)) /= 'A' then 0
    else if (getChar prob (row + dr * 3) (col + dc * 3)) /= 'S' then 0
    else 1


dirs : List (Int, Int)
dirs =
    tail $ (,) <$> [0, -1, 1] <*> [0, -1, 1]


part1 : Problem -> Int 
part1 (P size text) = go 0 0 0
    where
        go : Int -> Int -> Int -> Int
        go acc row col =
            if (row == size) then acc
            else if (col == size) then go acc (row + 1) 0
            else
                let
                    acc' = foldl (+) acc $ map (check1 (P size text) row col) dirs
                in
                go acc' row (col + 1)


patterns : List (Char, Char, Char, Char)
patterns =
    ('M', 'M', 'S', 'S') ::
    ('S', 'S', 'M', 'M') ::
    ('S', 'M', 'M', 'S') ::
    ('M', 'S', 'S', 'M') ::
    []


check2 : Problem -> Int -> Int -> (Char, Char, Char, Char) -> Int
check2 prob row col (c1, c2, c3, c4) =
    if (getChar prob row col) /= 'A' then 0
    else if (getChar prob (row - 1) (col - 1)) /= c1 then 0
    else if (getChar prob (row - 1) (col + 1)) /= c2 then 0
    else if (getChar prob (row + 1) (col + 1)) /= c3 then 0
    else if (getChar prob (row + 1) (col - 1)) /= c4 then 0
    else 1


part2 : Problem -> Int 
part2 (P size text) = go 0 0 0
    where
        go : Int -> Int -> Int -> Int
        go acc row col =
            if (row == size) then acc
            else if (col == size) then go acc (row + 1) 0
            else
                let
                    acc' = foldl (+) acc $ map (check2 (P size text) row col) patterns
                in
                go acc' row (col + 1)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let lines := forget $ split (=='\n') content
        let size := length lines
        printLn size

        printLn $ "Part One result: " ++ show (part1 $ P (cast size) (joinBy "" lines))
        printLn $ "Part Two result: " ++ show (part2 $ P (cast size) (joinBy "" lines))


main : IO ()
main = do
    run "aoc2024/day04/test.txt"
    run "aoc2024/day04/input.txt"
