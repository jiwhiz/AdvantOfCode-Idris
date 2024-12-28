module Main

import Data.List
import Data.Maybe
import Data.String
import Util


wins : (Integer, Integer) -> Integer
wins (time, distance) = time - 2 * (threshold 0) + 1
    where 
        threshold : Integer -> Integer
        threshold i = if (time - i) * i > distance then i else threshold (i + 1)


part1 : String -> String -> Integer
part1 t d =
    foldl (\acc, elem => acc * wins elem) 1 $ zip (parseIntegers t) (parseIntegers d)


parseCombinedNumber : String -> Integer
parseCombinedNumber str = fromMaybe 0 $ parseInteger $ pack $ filter (not . isSpace) $ unpack str 

part2 : String -> String -> Integer
part2 t d =
    wins (parseCombinedNumber t, parseCombinedNumber d)


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let (t::d::_) := lines content | _ => printLn "Not enough data"
    let times = snd $ break (isSpace) t
    let distances = snd $ break (isSpace) d
    putStrLn $ "Part One result: " ++ (show $ part1 times distances)
    putStrLn $ "Part Two result: " ++ (show $ part2 times distances)


main : IO ()
main = do
    run "aoc2023/day06/test.txt"
    run "aoc2023/day06/input.txt"
