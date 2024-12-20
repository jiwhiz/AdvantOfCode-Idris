module Main

import Data.List
import Data.List1
import Data.String
import Util
import BST


parseData : List String -> Maybe (List (Integer, List Integer))
parseData [] = Just []
parseData (l::ls) = 
    let
        Just num = parseInteger (fst $ break (== ':') l) | _ => Nothing
        numList = parseIntegers $ snd $ break (== ':') l
        Just rest = parseData ls | _ => Nothing
    in Just ((num, numList) :: rest)


check1 : Integer -> List Integer -> Integer
check1 sum numList = if go numList 0 then sum else 0
    where
        go : List Integer -> Integer -> Bool
        go [] acc = (acc == sum)
        go (x::xs) acc = 
            if sum < acc then False
            else if go xs (acc + x) then True
            else go xs (acc * x)
            


part1 : List (Integer, List Integer) -> Integer 
part1 [] = 0
part1 ( (sum, numList) :: rest) = 
    check1 sum numList + part1 rest


check2 : Integer -> List Integer -> Integer
check2 sum numList = if go numList 0 then sum else 0
    where
        combine : Integer -> Integer -> Integer
        combine n1 n2 =
            let str = show n1 ++ show n2
            in case parseInteger str of
                Nothing => 0
                Just n => n

        go : List Integer -> Integer -> Bool
        go [] acc = (acc == sum)
        go (x::xs) acc = 
            if sum < acc then False
            else if go xs (acc + x) then True
            else if go xs (acc * x) then True
            else go xs (combine acc x)


part2 : List (Integer, List Integer) -> Integer 
part2 [] = 0
part2 ( (sum, numList) :: rest) = 
    check2 sum numList + part2 rest


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let Just records := parseData (forget $ split (=='\n') content) | Nothing => printLn "parse data error"

        printLn $ "Part One result: " ++ show (part1 records)
        printLn $ "Part Two result: " ++ show (part2 records)


main : IO ()
main = do
    run "aoc2024/day07/test.txt"
    run "aoc2024/day07/input.txt"
