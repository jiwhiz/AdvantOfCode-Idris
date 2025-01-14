module Main

import Data.List
import Util


part2 : List Char -> Int
part2 list = go 0 0 list
    where
        go : Int -> Int -> List Char -> Int
        go count acc [] = if acc == -1 then count else 0
        go count acc (x::xs) =
            if acc == -1 then count
            else case x of
                '(' => go (count + 1) (acc + 1) xs
                ')' => go (count + 1) (acc - 1) xs
                _ => 0


run : String -> IO ()
run filename =
    do
        printLn filename
        list <- unpack <$> Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ sum $ map (\case {'(' => 1; ')' => -1; _ => 0}) list)
        putStrLn $ "Part Two result: " ++ (show $ part2 $ list)

main : IO ()
main = do
    run "aoc2015/day01/test.txt"
    run "aoc2015/day01/input.txt"
