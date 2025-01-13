module Main

import Data.Bits
import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Data.Queue
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.String.Extra
import Control.Monad.State
import Util
import Debug.Trace


parseData : List String -> List (String, String)
parseData [] = []
parseData (x::xs) = 
    let
        (n1, str) = break (== ':') x
        ns = words $ drop 1 str
        pairs = build n1 ns
    in pairs ++ parseData xs
    where
        build : String -> List String -> List (String, String)
        build _ [] = []
        build n1 (n2::rest) = if n1 < n2 then (n1, n2) :: build n1 rest else (n2, n1) :: build n1 rest


buildGraph : List (String, String) -> SortedMap String ( SortedSet String ) -> SortedMap String ( SortedSet String )
buildGraph [] map = map
buildGraph ((c1, c2)::xs) map = 
    buildGraph xs $ (add c1 c2 . add c2 c1) map
    where
        add : String -> String -> SortedMap String ( SortedSet String ) -> SortedMap String ( SortedSet String )
        add from to map =
            case lookup from map of
                Nothing => insert from (singleton to) map 
                Just set => insert from (insert to set) map


checkGroup : List (String, String) -> List (SortedSet String)
checkGroup pairs = go [] pairs
    where
        merge : SortedSet String -> List (SortedSet String) -> List (SortedSet String)
        merge set [] = [set]
        merge set (x::rest) =
            if isCons $ SortedSet.toList $ intersection set x
            then merge (union set x) rest
            else x :: merge set rest

        go : List (SortedSet String) -> List (String, String) -> List (SortedSet String)
        go sets [] = sets
        go sets ((l, r)::xs) = go (merge (insert l $ singleton r) sets) xs


part1 : List (String, String) -> Maybe (List (SortedSet String))
part1 pairs =
    head' $ filter (\l => length l == 2) $ -- brutal force using list comprehension
        [ checkGroup $ filter (\p => p /= p1 && p /= p2 && p /= p3) pairs |
            p1 <- pairs,
            p2 <- pairs,
            p3 <- pairs,
            p1 < p2,
            p2 < p3
        ]


findCuts : List (String, String) -> List (String, String)
findCuts pairs =
    let connMap = buildGraph pairs SortedMap.empty
        routeList = concat $ map (searchRoute connMap) $ genPairs pairs
        edgeMap = countEdges routeList
    in map (\(p, c) => p) $ sortBy (\(p1, c1), (p2, c2) => compare c2 c1) $ SortedMap.toList edgeMap
    where
        countEdges : List (String, String) -> SortedMap (String, String) Int
        countEdges =
            foldl
                (\m, (l, r) =>
                    let p = if l < r then (l, r) else (r, l)
                    in case lookup p m of
                        Nothing => insert p 1 m
                        Just count => insert p (count + 1) m
                )
                SortedMap.empty

        genPairs : List (String, String) -> List (String, String)
        genPairs pairs =
                let components = SortedSet.toList $ foldl (\s, (l, r) => insert l (insert r s)) SortedSet.empty pairs
                in go 700 components []
                where
                    go : Nat -> List String -> List (String, String) -> List (String, String)
                    go _ [] acc = acc
                    go _ [x] acc = acc
                    go Z _ acc = acc
                    go (S k) (x1::x2::rest) acc = go k rest ((x1, x2) :: acc)

        searchRoute : SortedMap String ( SortedSet String ) -> (String, String) -> List (String, String)
        searchRoute connMap (start, end) =
            go (Queue.singleton (start, []))
            where
                go : Queue (String, List (String, String)) -> List (String, String)
                go queue =
                    let Just ((top, route), queue') = dequeue queue | _ => [] -- error
                    in
                        if top == end then reverse route -- reach end
                        else case lookup top connMap of
                            Nothing => [] -- error
                            Just set =>
                                if contains end set then (top, end) :: route
                                else go (foldl (\q, c => enqueue (c, (top, c)::route) q) queue' $ SortedSet.toList set)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let pairs := sort (parseData $ lines content)
        let (p1::p2::p3::_) = findCuts pairs | _ => putStrLn "No result"
        let (s1::s2::_) := checkGroup $ filter (\p => p /= p1 && p /= p2 && p /= p3) pairs | _ => putStrLn "No result"
        putStrLn $ "Part One result: " ++ (show $ (length $ SortedSet.toList s1) * (length $ SortedSet.toList s2))


main : IO ()
main = do
    -- run "aoc2023/day25/test.txt"
    run "aoc2023/day25/input.txt"
