module Main

import Data.List1
import Data.String
import Util


data BST : Type -> Type where
    Empty : BST a
    Node : a -> BST a -> BST a -> BST a



insert : Ord a => a -> BST a -> BST a 
insert x Empty = Node x Empty Empty
insert x (Node y left right) =
    if x < y then
        Node y (insert x left) right
    else
        Node y left (insert x right)



inOrder : BST a -> List a
inOrder Empty = []
inOrder (Node x left right) =
    inOrder left ++ [x] ++ inOrder right 


parseIntegerPair : String -> Maybe (Integer, Integer)
parseIntegerPair l =
    case parseIntegers l of
        x::y::xs => Just (x, y)
        _ => Nothing

sort2Lists : List String -> (BST Integer, BST Integer)
sort2Lists lines =
    foldl
        (\(l1, l2), line =>
            case parseIntegerPair line of
                Just (n1, n2) => (insert n1 l1, insert n2 l2)
                Nothing => (l1, l2)
        )
        (Empty, Empty)
        lines


part1 : List Integer -> List Integer -> Integer
part1 l1 l2 =
    sum $ zipWith (\a, b => abs (a - b)) l1 l2


part2 : List Integer -> List Integer -> Integer
part2 l1 l2 =
    let
        countnum : Integer -> List Integer -> Integer
        countnum num [] = 0
        countnum num (x::xs) =
            if (num == x) then num + (countnum num xs)
            else 0

        score : Integer -> List Integer -> List Integer -> Integer
        score s [] _ = s
        score s _ [] = s
        score s (x :: xs) (y :: ys) =
            if (x == y) then
                score (s + (countnum x (y::ys))) xs (y::ys)
            else if (x < y) then
                score s xs (y::ys)
            else
                score s (x::xs) ys 

    in
        score 0 l1 l2


run : String -> IO ()
run filename =
    do
        printLn filename
        
        content <- Util.loadFile filename
        let (l1, l2) := sort2Lists $ forget $ String.split (=='\n') content

        printLn $ "Part One result: " ++ 
            (show $ part1 (inOrder l1) (inOrder l2))

        printLn $ "Part Two result: " ++ 
            (show $ part2 (inOrder l1) (inOrder l2))


main : IO ()
main = do
    run "aoc2024/day01/test.txt"
    run "aoc2024/day01/input.txt"
