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


data Gate = AND | OR | XOR

bitOper : Gate -> Int -> Int -> Int
bitOper AND i j = if i==1 && j == 1 then 1 else 0
bitOper OR i j = if i==1 || j == 1 then 1 else 0
bitOper XOR i j = if i /= j then 1 else 0

Show Gate where
    show AND = " AND "
    show OR = " OR "
    show XOR = " XOR "


Eq Gate where
    (==) AND AND = True
    (==) OR OR = True
    (==) XOR XOR = True
    (==) _ _ = False

record Inst where
    constructor MkInst
    gete : Gate
    in1 : String
    in2 : String
    out : String


Show Inst where
    show inst = "Inst[" ++ inst.in1 ++ (show inst.gete) ++ inst.in2 ++ " = " ++ inst.out ++"]"


parseConnections : List String -> List Inst
parseConnections [] = []
parseConnections (x::xs) =
    let (left::oper::right::arr::out::[]) = words x | _ => parseConnections xs
        Just gete = parseGate oper | _ => parseConnections xs
    in (MkInst gete left right out) :: parseConnections xs
    where
        parseGate : String -> Maybe Gate
        parseGate "AND" = Just AND
        parseGate "OR" = Just OR
        parseGate "XOR" = Just XOR
        parseGate _ = Nothing


iterate : List Inst -> List String -> SortedMap String Int -> SortedMap String Int
iterate insts zs map =
    if valuesReady zs map then map
    else iterate insts zs (step insts map)
    where
        valuesReady : List String -> SortedMap String Int -> Bool
        valuesReady [] m = True
        valuesReady (x::xs) m =
            case lookup x m of
                Nothing => False
                Just _ => valuesReady xs m

        step : List Inst -> SortedMap String Int -> SortedMap String Int
        step [] m = m
        step ((MkInst gate in1 in2 out)::xs) m =
            let Just v1 = lookup in1 m | Nothing => step xs m
                Just v2 = lookup in2 m | Nothing => step xs m
                v = bitOper gate v1 v2
           in step xs (insert out v m)

bitListToInt : List Int -> Int
bitListToInt [] = 0
bitListToInt [x] = x
bitListToInt (x::xs) = (bitListToInt xs) * 2 + x


loadZWires : List Inst -> List String
loadZWires insts = sort $ SortedSet.toList $ go insts empty
    where
        go : List Inst -> SortedSet String -> SortedSet String
        go [] set = set
        go ((MkInst gate in1 in2 out) :: xs) set =
            let set1 = if isPrefixOf "z" in1 then (insert in1 set) else set
                set2 = if isPrefixOf "z" in2 then (insert in2 set1) else set1
                set3 = if isPrefixOf "z" out then (insert out set2) else set2
            in go xs set3


loadZValues : List String -> SortedMap String Int -> List Int
loadZValues [] map = []
loadZValues (x::xs) map =
    case lookup x map of 
        Nothing => loadZValues xs map
        Just v => v :: loadZValues xs map


part1 : List (String, Int) -> List Inst -> Int
part1 initValues insts =
    let zs = loadZWires insts
        finalData = iterate insts zs (SortedMap.fromList initValues)
    in bitListToInt $ loadZValues zs finalData


part2 : List Inst -> String
part2 insts =
    joinBy "," $ sort $ map (\inst => inst.out) (checkRules insts)
    where
        notFound : String -> Gate -> List Inst -> Bool
        notFound _ _ [] = True
        notFound input gate ((MkInst gate' in1 in2 out)::xs) =
            if gate == gate' && (input == in1 || input == in2) then False
            else notFound input gate xs

        checkRules : List Inst -> List Inst
        checkRules [] = []
        checkRules (inst@(MkInst gate in1 in2 out)::xs) = checkRules xs ++
            if gate == XOR then
                if isPrefixOf "z" out then []
                else if (isPrefixOf "x" in1 || isPrefixOf "y" in1)
                    && (isPrefixOf "x" in2 || isPrefixOf "y" in2) then
                    if notFound out XOR insts then [inst]
                    else []
                else [inst]
            else if gate == OR then
                if isPrefixOf "z" out && out /= "z45" then [inst]
                else []
            else if gate == AND then
                if isPrefixOf "z" out then [inst]
                else if in1 /= "x00" && in1 /= "y00" && notFound out OR insts then [inst]  -- 
                else []
            else []


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (p1, p2) := break (=="") $ lines content
        let initValues := map (\(f, s) => (f, (fromMaybe 0 . parseInteger . snd . break (== ' ')) s )) $ map (break (== ':')) p1
        let insts := parseConnections p2
        printLn $ "Part One result: " ++ (show $ part1 initValues insts)
        printLn $ "Part Two result: " ++ (show $ part2 insts)
        pure ()


main : IO ()
main = do
    -- run "aoc2024/day24/test1.txt"
    -- run "aoc2024/day24/test2.txt"
    run "aoc2024/day24/input.txt"
