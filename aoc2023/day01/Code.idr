module Main

import Data.List
import Data.Maybe
import Data.String
import Util

-- PART One

partOne : List String -> Integer
partOne [] = 0
partOne (x::xs) =
    fromMaybe 0 (calibrate $ unpack x) + (partOne xs)
    where
        calibrate : List Char -> Maybe Integer
        calibrate chars = do
            first <- find isDigit chars
            last <- find isDigit (reverse chars)
            parseInteger $ pack [first, last]

-- PART TWO

digitsList : List ((List Char), Char)
digitsList =
    [ (['1'], '1')
    , (['2'], '2')
    , (['3'], '3')
    , (['4'], '4')
    , (['5'], '5')
    , (['6'], '6')
    , (['7'], '7')
    , (['8'], '8')
    , (['9'], '9')
    , (unpack "one", '1')
    , (unpack "two", '2')
    , (unpack "three", '3')
    , (unpack "four", '4')
    , (unpack "five", '5')
    , (unpack "six", '6')
    , (unpack "seven", '7')
    , (unpack "eight", '8')
    , (unpack "nine", '9')
    ]

findFirst : List Char -> Maybe Char
findFirst [] = Nothing
findFirst cs@(x :: xs) =
    case find (\digits => isPrefixOf (fst digits) cs) digitsList of
        Nothing => findFirst xs
        (Just y) => Just (snd y)

findLast : List Char -> Maybe Char
findLast [] = Nothing
findLast cs@(x :: xs) =
    case find (\digits => isSuffixOf (fst digits) (reverse cs)) digitsList of
        Nothing => findLast xs
        (Just y) => Just (snd y)


partTwo : List String -> Integer
partTwo [] = 0
partTwo (x::xs) =
    fromMaybe 0 (calibrate $ unpack x) + (partTwo xs)
    where
        calibrate : List Char -> Maybe Integer
        calibrate chars = do
            first <- findFirst chars
            last <- findLast (reverse chars)
            parseInteger $ pack [first, last]


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    printLn $ "Part One result: " ++ (show $ partOne $ lines content)
    printLn $ "Part Two result: " ++ (show $ partTwo $ lines content)


main : IO ()
main = do
    run "aoc2023/day01/test1.txt"
    run "aoc2023/day01/test2.txt"
    run "aoc2023/day01/input.txt"
