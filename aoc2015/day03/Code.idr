module Main

import Data.List
import Data.String
import Data.SortedSet
import Util


step : (Int, Int) -> Char -> (Int, Int)
step (row, col) c =
    case c of
        '^' => (row-1, col)
        'v' => (row+1, col)
        '<' => (row, col-1)
        '>' => (row, col+1)
        _   => (row, col)


part1 : List Char -> Nat
part1 steps =
    let start = (0, 0) in
    length $ SortedSet.toList $ go start steps (SortedSet.singleton start)
    where
        go : (Int, Int) -> List Char -> SortedSet (Int, Int) -> SortedSet (Int, Int)
        go _ [] set = set
        go position (x::xs) set =
            let next = step position x
            in go next xs (insert next set)


part2 : List Char -> Nat
part2 steps =
    let start = (0, 0) in
    length $ SortedSet.toList $ go start start steps (SortedSet.singleton start)
    where
        go : (Int, Int) -> (Int, Int) -> List Char -> SortedSet (Int, Int) -> SortedSet (Int, Int)
        go _ _ [] set = set
        go position _ [x] set =
            let next = step position x
            in insert next set
        go pos1 pos2 (x1::x2::xs) set =
            let next1 = step pos1 x1
                next2 = step pos2 x2
            in go next1 next2 xs (insert next1 (insert next2 set))


run : String -> IO ()
run filename =
    do
        printLn filename
        steps <- unpack <$> Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ part1 steps)
        putStrLn $ "Part Two result: " ++ (show $ part2 steps)
        pure ()


main : IO ()
main = do
    run "aoc2015/day03/test.txt"
    run "aoc2015/day03/input.txt"
