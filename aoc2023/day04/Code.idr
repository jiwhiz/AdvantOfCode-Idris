module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Util


parseNumbers : String -> Maybe (List Integer)
parseNumbers input =
    foldlM
        (\results, numStr => do
            num <- parseInteger $ trim numStr
            pure (num :: results)
        )
        []
        (words input)


getMatches : List Integer -> List Integer -> Integer
getMatches wins nums =
    foldl
        (\acc, num =>
            case find (==num) wins of
                Just _ => acc + 1
                Nothing => acc
        )
        0
        nums


||| parse one line of card and return matched numbers count
parseCardLine : String -> Integer
parseCardLine line = 
    let result := do
        numbersLine <- head' $ tail $ split (==':') line
        winNumbers <- parseNumbers $ head $ split (=='|') numbersLine
        mineNumbersStr <- head' $ tail $ split (=='|') numbersLine
        mineNumbers <- parseNumbers mineNumbersStr
        pure (winNumbers, mineNumbers)
    in
        case result of
            Nothing => 0
            Just (wins, nums) => getMatches wins nums


parseData : List String -> List Integer
parseData [] = []
parseData (x :: xs) = parseCardLine x :: (parseData xs)


getPoints : Integer -> Integer
getPoints 0 = 0
getPoints 1 = 1
getPoints m = 2 * getPoints (m-1)

partOne : List Integer -> Integer
partOne matches =
    foldl (\acc, m => acc + getPoints m) 0 matches


updateCopies : Integer -> Integer -> List Integer -> List Integer
updateCopies _ _ [] = []
updateCopies 0 cardCopy (x :: xs) = x :: xs
updateCopies matches cardCopy (x :: xs) = cardCopy + x :: updateCopies (matches - 1) cardCopy xs


calculate : List Integer -> List Integer -> List Integer
calculate (m :: ms) (x :: xs) = x :: (calculate ms $ updateCopies m x xs)
calculate _ _ = []


initCopies : List Integer -> List Integer
initCopies [] = []
initCopies (x :: xs) = 1 :: initCopies xs


partTwo : List Integer -> Integer
partTwo matches = foldl (+) 0 (calculate matches $ initCopies matches)


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let matches := parseData $ lines content
    printLn $ "Part One result: " ++ (show $ partOne matches)
    printLn $ "Part Two result: " ++ (show $ partTwo matches)


main : IO ()
main = do
    run "aoc2023/day04/test.txt"
    run "aoc2023/day04/input.txt"
