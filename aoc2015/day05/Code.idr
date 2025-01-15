module Main

import Data.List
import Data.String
import Util


part1 : List String -> Int
part1 [] = 0
part1 (x::xs) =
    let chars = unpack x in
    if countVowels chars >= 3 && hasDouble chars && (not . hasNaughtyStr) chars then 1 + part1 xs
    else part1 xs
    where
        countVowels : List Char -> Nat
        countVowels list = length $ filter id $ map (\case {'a' => True; 'e' => True; 'i' => True; 'o' => True; 'u' => True; _ => False}) list

        hasDouble : List Char -> Bool
        hasDouble [] = False
        hasDouble [x] = False
        hasDouble (x1::x2::rest) =
            if x1 == x2 then True
            else hasDouble (x2::rest)

        hasNaughtyStr : List Char -> Bool
        hasNaughtyStr [] = False
        hasNaughtyStr [x] = False
        hasNaughtyStr ('a'::'b'::_) = True
        hasNaughtyStr ('c'::'d'::_) = True
        hasNaughtyStr ('p'::'q'::_) = True
        hasNaughtyStr ('x'::'y'::_) = True
        hasNaughtyStr (_::x::xs) = hasNaughtyStr (x::xs)


part2 : List String -> Int
part2 [] = 0
part2 (x::xs) =
    let chars = unpack x in
    if hasNice1 chars && hasNice2 chars then 1 + part2 xs
    else part2 xs
    where
        hasNice1 : List Char -> Bool
        hasNice1 [] = False
        hasNice1 [x] = False
        hasNice1 (x1::x2::rest) =
            appear x1 x2 rest || hasNice1 (x2::rest)
            where
                appear : Char -> Char -> List Char -> Bool
                appear _ _ [] = False
                appear _ _ [v] = False
                appear x1 x2 (v1::v2::vs) =
                    if x1 == v1 && x2 == v2 then True
                    else appear x1 x2 (v2::vs)

        hasNice2 : List Char -> Bool
        hasNice2 [] = False
        hasNice2 [x] = False
        hasNice2 [x1, x2] = False
        hasNice2 (x1::x2::x3::rest) =
            if x1 == x3 then True
            else hasNice2 (x2::x3::rest)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ part1 $ lines content)
        putStrLn $ "Part Two result: " ++ (show $ part2 $ lines content)

main : IO ()
main = do
    run "aoc2015/day05/test1.txt"
    run "aoc2015/day05/test2.txt"
    run "aoc2015/day05/input.txt"
