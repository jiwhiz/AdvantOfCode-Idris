module Main

import Data.Nat
import Data.List1
import Data.String
import Data.SortedMap
import Data.SortedSet
import Util

parseInt : String -> Int
parseInt str =
    case parseInteger str of
        Just i => i
        Nothing => 0


parseRule : String -> SortedMap Int (SortedSet Int) -> SortedMap Int (SortedSet Int)
parseRule line rules =
    let 
        l1 := split (== '|') line
    in
    merge rules (SortedMap.singleton (parseInt (head l1)) (SortedSet.singleton (parseInt (head $ reverse l1)))) 


parseUpdate : String -> List Int
parseUpdate l =
    map parseInt $ forget $ split (== ',') l


parseData : List String -> (SortedMap Int (SortedSet Int), List (List Int))
parseData lines = go lines True SortedMap.empty []
    where
        go : List String -> Bool -> SortedMap Int (SortedSet Int) -> List (List Int) -> (SortedMap Int (SortedSet Int), List (List Int))
        go [] loadRule rules updates = (rules, updates)
        go (l::lines) loadRule rules updates =
            if strLength l == 0 then 
                go lines (not loadRule) rules updates
            else if loadRule then
                go lines loadRule (parseRule l rules) updates
            else
                go lines loadRule rules (parseUpdate l :: updates)


middlePage : List Int -> List Int -> Int
middlePage _ [] = 0
middlePage left (x::xs) =
    if (length left == length xs) then x
    else middlePage (x::left) xs


isValid : SortedMap Int (SortedSet Int) -> List Int -> List Int -> Bool
isValid rules left [] = True
isValid rules left (x::xs) =
    case lookup x rules of  
        Just s => 
            if isNil $ SortedSet.toList $ intersection s (fromList left) then
                isValid rules (x::left) xs
            else False
        Nothing => isValid rules (x::left) xs


fix : SortedMap Int (SortedSet Int) -> List Int -> List Int
fix rules update =
    sortBy
        (\x, y =>
            case (lookup x rules, lookup y rules) of
                (Nothing, Nothing) => EQ
                (Just s, Nothing) => if contains y s then LT else EQ 
                (Nothing, Just s) => if contains x s then GT else EQ
                (Just s1, Just s2) => if contains y s1 then LT else if contains x s2 then GT else EQ
        )
        update


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let (rules, updates) := parseData $ forget $ split (=='\n') content

        printLn $ "Part One result: " ++ show (sum $ map (middlePage []) $ filter (isValid rules []) updates)
        printLn $ "Part Two result: " ++ show (sum $ map (middlePage [] . fix rules) $ filter (not . isValid rules []) updates)


main : IO ()
main = do
    run "aoc2024/day05/test.txt"
    run "aoc2024/day05/input.txt"
