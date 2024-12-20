module Main

import Data.List1
import Data.String
import Util

checkSafe : Integer -> List Integer -> Bool
checkSafe _ [] = False
checkSafe d [x] =
    if d == 0 then
        False
    else 
        True
checkSafe d (x::y::xs) = 
    if x > y then
        if d > 0 then 
            False
        else if x - y > 3 then
            False
        else 
            checkSafe (y-x) (y::xs)
    else if x < y then
        if d < 0 then
            False
        else if y - x > 3 then  
            False
        else
            checkSafe (y-x) (y::xs)
    else
        False


part1 : List String -> Integer
part1 lines =
    foldl
        (\acc, line =>
            if checkSafe 0 (parseIntegers line) then
                acc + 1
            else
                acc
        )
        0
        lines


generateList : List Integer -> List (List Integer)
generateList [] = []
generateList (x :: xs) =
    xs :: (map (x ::) (generateList xs))


checkLists : List (List Integer) -> Bool
checkLists [] = False
checkLists (x :: xs) =
    if checkSafe 0 x then
        True 
    else
        checkLists xs



part2 : List String -> Integer
part2 lines =
    foldl
        (\acc, line =>
            let
                list = parseIntegers line
            in
            if checkLists (list :: generateList list) then
                acc + 1
            else
                acc

        )
        0
        lines


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let lines := forget $ split (=='\n') content

        printLn $ "Part One result: " ++ (show $ part1 lines)
        printLn $ "Part Two result: " ++ (show $ part2 lines)


main : IO ()
main = do
    run "aoc2024/day02/test.txt"
    run "aoc2024/day02/input.txt"
