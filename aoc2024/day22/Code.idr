module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.String
import Control.Monad.State
import Util


parseData : List String -> List Int 
parseData [] = []
parseData (x::xs) =
    case parseInteger x of 
        Nothing => parseData xs
        Just i => cast i :: parseData xs


xor : List Int -> List Int -> List Int
xor [] [] = []
xor l1 [] = l1
xor [] l2 = l2
xor (x::xs) (y::ys) = (if x == y then 0 else 1) :: xor xs ys


bitListToInt : List Int -> Int
bitListToInt [] = 0
bitListToInt [x] = x
bitListToInt (x::xs) = (bitListToInt xs) * 2 + x



intToBitList : Int -> List Int
intToBitList n =
    if n <=0 then []
    else (n `mod` 2) :: intToBitList (n `div` 2)


shiftRight : Nat -> List Int -> List Int 
shiftRight n l = drop n l


shiftLeft : Nat -> List Int -> List Int 
shiftLeft Z l = l
shiftLeft (S k) l = 0 :: shiftLeft k l

prune : List Int -> List Int
prune list = take 24 list


pseudoNext : Int -> Int
pseudoNext input =
    let p0 = intToBitList input
        p1 = prune $ xor p0 (shiftLeft 6 p0) 
        p2 = prune $ xor p1 (shiftRight 5 p1)
        p3 = prune $ xor p2 (shiftLeft 11 p2) 
    in bitListToInt p3


repeatN : Nat -> (a -> a) -> a -> a
repeatN Z f x = x
repeatN (S n) f x = repeatN n f (f x)

part1 : List Int -> List Int
part1 [] = []
part1 (num::rest) =
    (repeatN 2000 pseudoNext num) :: part1 rest


FourSeq : Type
FourSeq = (Int, Int, Int, Int)

catchSeq : Int -> SortedMap (Int, Int, Int, Int) Int -> SortedMap (Int, Int, Int, Int) Int
catchSeq number cache =
    mergeWith (+) cache $ fst
        (foldl 
            (\(c, num, seq), _ =>
                let num' = pseudoNext num
                    price = num `mod` 10
                    price' = num' `mod` 10
                    diff = price' - price
                    seq' = Prelude.(::) diff seq
                in case seq' of
                    (d1::d2::d3::d4::[]) => 
                        case lookup (d1,d2,d3,d4) c of
                            Nothing => (insert (d1,d2,d3,d4) price' c, num', (d1::d2::d3::[])) -- save first occurred seq with price
                            Just _ =>(c, num', (d1::d2::d3::[]))  -- already appeared before, ignore this one
                    
                    _ => (c, num', seq')
            )
            (empty, number, []) 
            (replicate 2000 ())
        )

part2 : List Int -> Int
part2 numbers =
    let cache =
        foldl
            (\c, num =>
                catchSeq num c
            )
            empty
            numbers
    in foldl max 0 (values cache)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let numbers := parseData $ lines content
        printLn $ "Part One result: " ++ (show $ sum $ part1 numbers)
        printLn $ "Part Two result: " ++ (show $ part2 numbers)


main : IO ()
main = do
    run "aoc2024/day22/test.txt"
    run "aoc2024/day22/input.txt"
