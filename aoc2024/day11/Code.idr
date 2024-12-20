module Main

import Data.List
import Data.Nat
import Data.SortedMap
import Util
import Debug.Trace

even : Nat -> Bool
even Z = True                     -- zero is even
even (S Z) = False                -- one is not even
even (S (S n)) = even n           -- if we can remove two from n, its parity is unchanged

half : Nat -> Nat
half Z = Z                -- half of 0 is 0
half (S Z) = Z            -- half of 1 is 0 (integer division floors)
half (S (S n)) = S (half n)

blink : List (Integer, Int) -> List (Integer, Int)
blink [] = []
blink ((x, n)::xs) =
    let
        str = show x
        len = length str
    in
    if x == 0 then 
        (1, n) :: blink xs
    else if even len then
        ((\num => (num, n)) <$> (parseIntegers $ pack (take (half len) (unpack str) ++ [' '] ++ drop (half len) (unpack str)))) ++ blink xs
    else
        (x * 2024, n) :: blink xs


combine : SortedMap Integer Int -> List (Integer, Int) -> SortedMap Integer Int
combine map [] = map
combine map ((k, v)::xs) =
    case lookup k map of
        Nothing => combine (insert k v map) xs 
        Just v' => combine (insert k (v + v') map) xs


repeatN : Int -> SortedMap Integer Int -> Int
repeatN count map =
    let resultMap = go count map in foldl (+) 0 $ snd <$> SortedMap.toList resultMap 
    where
        go : Int -> SortedMap Integer Int -> SortedMap Integer Int
        go 0 m = m
        go n m = go (n-1) $ combine empty $ blink $ toList m


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        printLn $ "Part One result: " ++ (show $ repeatN 25 $ combine empty $ (\n => (n, 1)) <$> parseIntegers content)
        printLn $ "Part Two result: " ++ (show $ repeatN 75 $ combine empty $ (\n => (n, 1)) <$> parseIntegers content)


main : IO ()
main = do
    run "aoc2024/day11/test.txt"
    run "aoc2024/day11/input.txt"
