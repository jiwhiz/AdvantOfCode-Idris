module Main

import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Control.Monad.State
import Util


numPad : SortedMap Char (Int, Int)
numPad = fromList 
    [ ('A', (3,2))
    , ('0', (3,1))
    , ('1', (2,0))
    , ('2', (2,1))
    , ('3', (2,2))
    , ('4', (1,0))
    , ('5', (1,1))
    , ('6', (1,2))
    , ('7', (0,0))
    , ('8', (0,1))
    , ('9', (0,2))
    ]


walk : Char -> Int -> List Char
walk dir 0 = []
walk dir n = dir :: walk dir (n - 1)

moveNum : Char -> Char -> List (List Char)
moveNum n1 n2 =
    let Just (r1, c1) = lookup n1 numPad | _ => []
        Just (r2, c2) = lookup n2 numPad | _ => []
    in
    if r1 == r2 && c1 == c2 then [['A']]
    else if r1 == r2 && c1 < c2 then [walk '>' (c2-c1) ++ ['A']]
    else if r1 == r2 && c1 > c2 then [walk '<' (c1-c2) ++ ['A']]
    else if c1 == c2 && r1 < r2 then [walk 'v' (r2-r1) ++ ['A']]
    else if c1 == c2 && r1 > r2 then [walk '^' (r1-r2) ++ ['A']]
    else if r1 < r2 && c1 < c2 then
        if c1 == 0 && r2 == 3 then [walk '>' (c2-c1) ++ walk 'v' (r2-r1) ++ ['A']]
        else (walk '>' (c2-c1) ++ walk 'v' (r2-r1) ++ ['A']) :: [walk 'v' (r2-r1) ++ walk '>' (c2-c1) ++ ['A']]
    else if r1 < r2 && c1 > c2 then [walk 'v' (r2-r1) ++ walk '<' (c1-c2) ++ ['A']]
    else if r1 > r2 && c1 < c2 then 
        (walk '^' (r1-r2) ++ walk '>' (c2-c1) ++ ['A']) :: [walk '>' (c2-c1) ++ walk '^' (r1-r2) ++ ['A']]
    else if r1 > r2 && c1 > c2 then
        if r1 == 3 && c2 == 0 then [walk '^' (r1-r2) ++ walk '<' (c1-c2) ++ ['A']]
        else (walk '^' (r1-r2) ++ walk '<' (c1-c2) ++ ['A']) :: [walk '<' (c1-c2) ++ walk '^' (r1-r2) ++ ['A']]
    else [[]]


moveDir : Char -> Char -> List (List Char)
moveDir 'A' 'A' = [ [] ]
moveDir 'A' '^' = [ ['<'] ]
moveDir 'A' '>' = [ ['v'] ]
moveDir 'A' 'v' = [ ['<', 'v'], ['v', '<'] ]
moveDir 'A' '<' = [ ['v', '<', '<'], ['<', 'v', '<'] ]
moveDir '^' '^' = [ [] ]
moveDir '^' 'A' = [ ['>'] ]
moveDir '^' 'v' = [ ['v'] ]
moveDir '^' '>' = [ ['v', '>'], ['>', 'v'] ]
moveDir '^' '<' = [ ['v', '<']]
moveDir '>' '>' = [ [] ]
moveDir '>' 'A' = [ ['^'] ]
moveDir '>' 'v' = [ ['<'] ]
moveDir '>' '^' = [ ['<', '^'], ['^', '<'] ]
moveDir '>' '<' = [ ['<', '<'] ]
moveDir 'v' 'v' = [ [] ]
moveDir 'v' '<' = [ ['<'] ]
moveDir 'v' '>' = [ ['>'] ]
moveDir 'v' '^' = [ ['^'] ]
moveDir 'v' 'A' = [ ['>', '^'], ['^', '>'] ]
moveDir '<' '<' = [ [] ]
moveDir '<' 'v' = [ ['>'] ]
moveDir '<' '^' = [ ['>', '^'] ]
moveDir '<' '>' = [ ['>', '>'] ]
moveDir '<' 'A' = [ ['>', '>', '^'], ['>', '^', '>'] ]
moveDir _ _ = []


pressNumInstrs : List Char -> List (List Char)
pressNumInstrs [] = [[]]
pressNumInstrs [s] = [[]]
pressNumInstrs (s::x::xs) = [ steps ++ rest | steps <- moveNum s x, rest <- pressNumInstrs (x::xs)]


pressDirInstrs : List Char -> List (List Char)
pressDirInstrs [] = [[]]
pressDirInstrs [s] = [[]]
pressDirInstrs (s::x::xs) =
    [ steps ++ ['A'] ++ rest | steps <- moveDir s x, rest <- pressDirInstrs (x::xs)]



mutual 
    calculateMinLen : Int -> List Char -> State (SortedMap (List Char, Int) Integer) Integer
    calculateMinLen level charList =
        if level == 0 then
            pure $ (cast . length) charList
        else
            foldlM 
                (\acc, frag => do 
                    fragLen <- minLengthFragment level frag
                    pure (acc + fragLen)
                )
                0
                (fst $ List1.unsnoc $ split (== 'A') charList) -- remove last item of []


    minLengthFragment : Int -> List Char -> State (SortedMap (List Char, Int) Integer) Integer
    minLengthFragment level fragment = do
        memo <- get
        case lookup (fragment, level) memo of
            Just res =>
                pure res
            Nothing =>
                do
                    result <-
                        foldlM 
                            (\m, inst => do
                                minLength <- calculateMinLen (level-1) inst
                                pure $ min m minLength
                            )
                            99999999999
                            (pressDirInstrs ( 'A' :: fragment ++ ['A'] ))
                    modify (insert (fragment, level) result)
                    pure result

||| Brutal force
part1 : String -> Integer
part1 code =
    let l1 = pressNumInstrs $ 'A'::(unpack code)
        l2 = concatMap (pressDirInstrs . ((::) 'A')) l1
        l3 = concatMap (pressDirInstrs . ((::) 'A')) l2
        min = foldl min 99999999999999999999999 (map (cast . length) l3)
        Just num = parseInteger $ fst (break (== 'A') code) | _ => 0
    in num * min


part2 : Int -> String -> Integer
part2 level code =
    let list = pressNumInstrs ('A' :: (unpack code))
        l = evalState SortedMap.empty (traverse (calculateMinLen level) list)
        min = foldl min 99999999999999999999999 l
        Just num = parseInteger $ fst (break (== 'A') code) | _ => 0
    in num * min


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let codes := lines content
        printLn $ "Part One result: " ++ (show $ foldl (+) 0 (part2 2 <$> codes))
        printLn $ "Part Two result: " ++ (show $ foldl (+) 0 (part2 25 <$> codes))


main : IO ()
main = do
    run "aoc2024/day21/test.txt"
    run "aoc2024/day21/input.txt"
