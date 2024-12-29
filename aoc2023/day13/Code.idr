module Main

import Debug.Trace
import Data.Fin
import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.Vect
import Decidable.Equality
import Util


parseBlock : List String -> List (List (List Char))
parseBlock lines =
    map unpack <$> (forget $ split (== "") lines)

isMirror : List (List Char) -> List (List Char) -> Bool
isMirror [] [] = True
isMirror [] (x :: xs) = True
isMirror (x :: xs) [] = True
isMirror (x :: xs) (y :: ys) = if x == y then isMirror xs ys else False


hMirror : List (List Char) -> List (List Char) -> Nat
hMirror [] y = 0
hMirror [x] y = 0
hMirror (x1 :: x2 :: xs) y =
    if isMirror (x1::y) (x2::xs) then length (x1::y) else hMirror (x2::xs) (x1::y)

part1 : List (List Char) -> Nat
part1 block =
    let h := hMirror block []
        v := hMirror (transpose block) []
    in h*100+v


canSmudge : List Char -> List Char -> Nat -> Bool
canSmudge [] [] count = (count == 1)
canSmudge [] (x :: xs) count = (count == 1)
canSmudge (x :: xs) [] count = (count == 1)
canSmudge (x :: xs) (y :: ys) count = canSmudge xs ys (if x == y then count else S count)

isAlmostMirror : List (List Char) -> List (List Char) -> Bool -> Bool
isAlmostMirror [] [] smudged = smudged
isAlmostMirror [] (x :: xs) smudged = smudged
isAlmostMirror (x :: xs) [] smudged = smudged
isAlmostMirror (x :: xs) (y :: ys) smudged =
    if x == y then isAlmostMirror xs ys smudged 
    else if not smudged && canSmudge x y 0 then isAlmostMirror xs ys True
    else False


hMirror2 : List (List Char) -> List (List Char) -> Nat
hMirror2 [] y = 0
hMirror2 [x] y = 0
hMirror2 (x1 :: x2 :: xs) y =
    if isAlmostMirror (x1::y) (x2::xs) False then length (x1::y)
    else hMirror2 (x2::xs) (x1::y)


part2 : List (List Char) -> Nat
part2 block =
    let h := hMirror2 block []
        v := hMirror2 (transpose block) []
    in h*100+v


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let blocks := parseBlock $ lines content

    putStrLn $ "Part One result: " ++ (show $ sum $ (part1 <$> blocks))
    putStrLn $ "Part Two result: " ++ (show $ sum $ (part2 <$> blocks))


main : IO ()
main = do
    run "aoc2023/day13/test.txt"
    run "aoc2023/day13/input.txt"
