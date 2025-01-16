module Main


import Data.List
import Data.String
import Util


part1 : List String -> Int
part1 = sum . map (\str => strLength str - inMemorySize (unpack str))
    where
        inMemorySize : List Char -> Int
        inMemorySize [] = 0
        inMemorySize ('\\'::'\\'::rest) = 1 + inMemorySize rest
        inMemorySize ('\\'::'"'::rest) = 1 + inMemorySize rest
        inMemorySize ('\\'::'x'::rest) = 1 + inMemorySize (drop 2 rest)
        inMemorySize ('"'::xs) = inMemorySize xs
        inMemorySize (x::xs) = 1 + inMemorySize xs


part2 : List String -> Int
part2 = sum . map (\str => encodeSize (unpack str) - strLength str)
    where
        encodeSize : List Char -> Int
        encodeSize [] = 2 -- double quotes wrap the string
        encodeSize ('\\'::rest) = 2 + encodeSize rest
        encodeSize ('"'::rest) = 2 + encodeSize rest
        encodeSize (x::xs) = 1 + encodeSize xs


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ part1 $ lines content)
        putStrLn $ "Part Two result: " ++ (show $ part2 $ lines content)


main : IO ()
main = do
    run "aoc2015/day08/test.txt"
    run "aoc2015/day08/input.txt"
