module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Control.Monad.State
import Util


parseBlock : List String -> (Bool, List Int)
parseBlock [] = (False, [])
parseBlock (l0::rest) =
    if (isPrefixOf "#" l0) then (True, parse rest (replicate (length l0) 0))
    else (False, parse (tail (l0::rest)) (replicate (length l0) (-1)))
    where
        parseLine : List Char -> List Int -> List Int
        parseLine [] _ = []
        parseLine _ [] = []
        parseLine (c::cs) (i::is) =
            case c of
                '#' => (i+1) :: parseLine cs is
                _ => i :: parseLine cs is

        parse : List String -> List Int -> List Int
        parse [] heights = heights
        parse (x::xs) heights = parse xs $ parseLine (unpack x) heights

parseData : String -> (List (List Int), List (List Int))
parseData str =
    let schematics := parseBlock <$> (forget $ List.split (== "") $ lines str)
    in
        ( map snd $ filter (\(isLock, heights) => isLock) schematics
        , map snd $ filter (\(isLock, heights) => not isLock) schematics)


fit : List Int -> List Int -> Bool
fit [] [] = True
fit [] _  = True
fit _  [] = True
fit (k::ks) (l::ls) = if k+l > 5 then False else fit ks ls 


part1 : String -> Nat
part1 content =
    let (locks, keys) := parseData content in
    List.length $
        [ (lock, key) |
            lock <- locks,
            key <- keys,
            fit lock key
        ]


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (locks, keys) := parseData content
        printLn $ "Part One result: " ++ (show $ part1 content)


main : IO ()
main = do
    run "aoc2024/day25/test.txt"
    run "aoc2024/day25/input.txt"
