module Main


import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.String
import Control.Monad.State
import Util
import Debug.Trace


parsePatterns : String -> List String
parsePatterns str = map trim $ forget $ split (== ',') str


-- This is the core function that checks how many ways a single string can be built from the patterns.
-- It uses State to maintain a memo dictionary from String to Int.
checkDesign : List String -> String -> State (SortedMap String Int) Int
checkDesign patterns design = do
    memo <- get
    case lookup design memo of
        Just res => 
            -- If we have a cached result, just return it.
            pure res
        Nothing =>
            -- If not cached, we compute it.
            if strLength design == 0 then
                -- Empty string can always be built (by using zero patterns).
                do modify (insert design 1)
                   pure 1
            else
                do
                    -- For each pattern that is a prefix of design string, we try to recursively check the remainder of the string.
                    -- And add up results for them
                    result <- foldlM
                                (\acc, pattern =>
                                    do
                                    count <- checkDesign patterns (strSubstr (strLength pattern) (strLength design - strLength pattern) design)
                                    pure (acc + count)
                                ) 0 (filter (flip isPrefixOf design) patterns)
                    -- Store the computed result in the memo before returning.
                    modify (insert design result)
                    pure result


checkStrings : List String -> List String -> List Int
checkStrings patterns inputs = 
    evalState empty (traverse (checkDesign patterns) inputs)


run : String -> IO ()
run filename = do
    printLn filename
    content <- Util.loadFile filename

    let (l1::l2::ls) := lines content | _ => printLn "Missing patterns line"
    let patterns := parsePatterns l1
    let designs := ls
    printLn filename

    let results := checkStrings patterns designs
    printLn $ "Part 1 : " ++ (show $ length $ filter id $ map (\n => n > 0) $ results)
    printLn $ "Part 2 : " ++ (show $ sum $ results)


main : IO ()
main = do
    run "aoc2024/day19/test.txt"
    run "aoc2024/day19/input.txt"

