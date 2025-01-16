module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Util
import Debug.Trace

Graph : Type
Graph = SortedMap (String, String) Int


parseGraph : List String -> Graph
parseGraph lines = go lines empty
    where
        parseDistance : String -> Maybe (String, String, Int)
        parseDistance str =
            let (c1::_::c2::_::distStr::_) = words str | _ => Nothing
            in do
                dist <- parsePositive distStr
                pure (c1, c2, dist)

        go : List String -> Graph -> Graph
        go [] graph = graph
        go (x::xs) graph =
            case parseDistance x of
                Nothing => go xs graph
                Just (c1, c2, dist) => go xs (insert (c1, c2) dist (insert (c2, c1) dist graph))


permu : List a -> List (List a)
permu [] = [[]]
permu (x :: xs) = concatMap (insertEverywhere x) (permu xs)
    where
        insertEverywhere : a -> List a -> List (List a)
        insertEverywhere y [] = [[y]]
        insertEverywhere y (z :: zs) =
            (y :: z :: zs) :: map (z ::) (insertEverywhere y zs)


calculate : Int -> Graph -> List String -> Int
calculate acc _ [] = acc
calculate acc _ [x] = acc
calculate acc graph (x1::x2::rest) =
    case lookup (x1, x2) graph of
        Nothing => -1
        Just dist => calculate (acc + dist) graph (x2::rest)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let graph := parseGraph $ lines content
        let cities := foldl (\s, (c1, c2) => insert c1 (insert c2 s)) SortedSet.empty (keys graph)
        let (h::list) := filter ((<) 0 ) $ (calculate 0 graph <$> permu (SortedSet.toList cities)) | _ => putStrLn "No result"
        putStrLn $ "Part One result: " ++ (show $ foldl min h list)
        putStrLn $ "Part Two result: " ++ (show $ foldl max h list)


main : IO ()
main = do
    run "aoc2015/day09/test.txt"
    run "aoc2015/day09/input.txt"

