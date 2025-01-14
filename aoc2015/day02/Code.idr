module Main

import Data.List
import Data.List1
import Data.String
import Util


parseData : String -> List Int
parseData str = do
    sort $ mapMaybe id $ map parsePositive $ forget $ split (=='x') str


part1 : List Int -> Int
part1 boxSize =
    let (s1::s2::s3::_) = boxSize | _ => 0
    in 2 * s1 * s2 + 2 * s2 * s3 + 2 * s3 * s1 + s1 * s2


part2 : List Int -> Int
part2 boxSize =
    let (s1::s2::s3::_) = boxSize | _ => 0
    in 2 * s1 + 2 * s2 + s1 * s2 * s3


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let boxes := parseData <$> (lines content)
        putStrLn $ "Part One result: " ++ (show $ sum $ map part1 boxes)
        putStrLn $ "Part Two result: " ++ (show $ sum $ map part2 boxes)


main : IO ()
main = do
    run "aoc2015/day02/test.txt"
    run "aoc2015/day02/input.txt"
