module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Control.Monad.State
import Util


parseData : List String -> List (String, String)
parseData [] = []
parseData (x::xs) = 
    let
        (p1, p2) = break (== '-') x
        (p3, p4) = span (== '-') p2
    in
    (p1, p4) :: parseData xs


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


part1 : SortedMap String ( SortedSet String ) -> Nat
part1 connMap =
    List.length $ -- brutal force using list comprehension
        [ (c1, c2, c3) |
            (c1, conn1) <- SortedMap.toList connMap,
            (c2, conn2) <- SortedMap.toList connMap,
            (c3, conn3) <- SortedMap.toList connMap,
            contains c2 conn1,
            contains c3 conn1,
            contains c1 conn2,
            contains c3 conn2,
            contains c1 conn3,
            contains c2 conn3,
            (isPrefixOf "t" c1 || isPrefixOf "t" c2 || isPrefixOf "t" c3),
            c1 < c2,
            c2 < c3
        ]


-- | Bron–Kerbosch algorithmr.
-- | R : set of nodes in the current clique
-- | P : set of candidate nodes that can still join the clique
-- | X : set of nodes that must not join the clique
-- | graph : the graph
bronKerbosch : SortedSet String
            -> SortedSet String
            -> SortedSet String
            -> SortedMap String (SortedSet String)
            -> List (SortedSet String)
bronKerbosch setR setP setX graph =
    if isEmpty setP && isEmpty setX then
        -- No more candidates, no more exclusions => R is a maximal clique
        [setR]
    else
        let 
            -- Pick a pivot from P ∪ X.  
            -- For simplicity, we use head here; in real code you should handle the empty set carefully.
            pivotCandidate : String
            pivotCandidate = fromMaybe "???" $ head' $ SortedSet.toList $ SortedSet.union setP setX
            
            pivotNeighbors = lookupWithDefault SortedSet.empty pivotCandidate graph
            
            -- We explore only vertices in P that are NOT neighbors of the pivot.
            -- This is a standard pivoting trick to reduce recursive calls.
            toExplore = SortedSet.difference setP pivotNeighbors
            
            -- Fold over the vertices we are actually going to explore
            results =
                foldl
                    (\acc, v =>
                        let vNeighbors = lookupWithDefault SortedSet.empty v graph
                            newR = SortedSet.insert v setR
                            newP = SortedSet.intersection setP vNeighbors
                            newX = SortedSet.intersection setX vNeighbors
                            foundCliques = bronKerbosch newR newP newX graph
                            
                            -- After recursing, move 'v' from P to X
                            -- We'll do that outside the recursion, so we just gather results here
                        in acc ++ foundCliques
                    )
                    []
                    (SortedSet.toList toExplore)
        in results
    where 
        isEmpty : SortedSet String -> Bool
        isEmpty set = isNil $ SortedSet.toList set

        lookupWithDefault : b -> a -> SortedMap a b -> b 
        lookupWithDefault def key map =
            case SortedMap.lookup key map of
                Nothing => def
                (Just x) => x


part2 : SortedMap String ( SortedSet String ) -> String 
part2 graph =
    let 
        maximalCliques = bronKerbosch SortedSet.empty (SortedSet.fromList $ keys graph) SortedSet.empty graph
        largest = head' $ sortBy (\l1, l2 => compare (length l2) (length l1)) $ map SortedSet.toList maximalCliques
    in case largest of
        Nothing => ""
        Just l => joinBy "," $ sort l


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let graph := buildGraph (parseData $ lines content) SortedMap.empty
        printLn $ "Part One result: " ++ (show $ part1 graph)
        printLn $ "Part Two result: " ++ (show $ part2 graph)


testGraph : SortedMap String (SortedSet String)
testGraph = SortedMap.fromList
        [ ("A", SortedSet.fromList ["B", "C"])
        , ("B", SortedSet.fromList ["A", "C", "D"])
        , ("C", SortedSet.fromList ["A", "B", "D"])
        , ("D", SortedSet.fromList ["B", "C"])
        , ("E", SortedSet.empty)
        ]


main : IO ()
main = do
    run "aoc2024/day23/test.txt"
    run "aoc2024/day23/input.txt"
