module Main

import Data.List
import Data.Maybe
import Data.String
import Util


record NumData where
    constructor MkNumData
    row : Nat
    startCol : Nat
    endColPlus1 : Nat 
    value : Integer


implementation Show NumData where
    show v = (show v.value) ++ "(" ++ (show v.row) ++ ", " ++ (show v.startCol) ++ "-" ++ (show v.endColPlus1) ++ ")"


record SymbolData where
    constructor MkSymbolData
    row : Nat
    col : Nat
    value : Char


implementation Show SymbolData where
    show v = (show v.value) ++ "(" ++ (show v.row) ++ ", " ++ (show v.col) ++ ")"

adjacent : NumData -> SymbolData -> Bool
adjacent nd sd =
    (sd.row == nd.row && (S sd.col == nd.startCol || sd.col == nd.endColPlus1))
    || (S sd.row == nd.row && (S sd.col >= nd.startCol && sd.col <= nd.endColPlus1)) 
    || (sd.row == S nd.row && (S sd.col >= nd.startCol && sd.col <= nd.endColPlus1)) 

parseData : List String -> (List NumData, List SymbolData)
parseData lines =
    fst $ foldl
        (\((nums, syms), row), lineStr =>
            let (ns, ss) = parse row 0 [] 0 (unpack lineStr)
            in ((nums ++ ns, syms ++ ss), row + 1)
        )
        (([], []), 0)
        lines
    where
        parseNum : List Char -> Integer
        parseNum cl = fromMaybe 0 $ parseInteger $ pack cl

        parse : (r : Nat) -> (c : Nat) -> (numChars : List Char) -> (nsCol : Nat) -> List Char -> (List NumData, List SymbolData)
        parse r c [] _ [] = ([], [])
        parse r c [] _ ['\n'] = ([], [])
        parse r c [] _ (x :: xs) =
            if isDigit x then parse r (S c) [x] c xs
            else if x == '.' then parse r (S c) [] 0 xs
            else -- find a symbol
                let (nl, sl) = parse r (S c) [] 0 xs
                in (nl, (MkSymbolData r c x) :: sl)
        parse r c nstr@(x :: xs) nsCol [] = ([(MkNumData r nsCol c (parseNum nstr))], [])
        parse r c nstr@(x :: xs) nsCol ['\n'] = ([(MkNumData r nsCol c (parseNum nstr))], [])
        parse r c nstr@(x :: xs) nsCol (y :: ys) =
            if isDigit y then parse r (S c) (nstr ++ [y]) nsCol ys
            else if y /= '.' then 
                let (nl, sl) = parse r (S c) [] 0 ys 
                in ((MkNumData r nsCol c (parseNum nstr)) :: nl, (MkSymbolData r c y) :: sl)
            else
                let (nl, sl) = parse r (S c) [] 0 ys 
                in ((MkNumData r nsCol c (parseNum nstr)) :: nl, sl)


calculate : List NumData -> List SymbolData -> Integer 
calculate ndlst sdlst =
    foldl
        (\acc, nd =>
            case find (\sd => adjacent nd sd) sdlst of
                Nothing => acc
                Just _ => acc + nd.value
        )
        0
        ndlst


gearRatio : List NumData -> List SymbolData -> Integer 
gearRatio ndlst sdlst =
    foldl
        (\acc, sd =>
            case filter (\nd => adjacent nd sd) ndlst of
                fst :: snd :: [] => fst.value * snd.value + acc
                _ => acc
        )
        0
        sdlst


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let (nums, syms) := parseData $ lines content
    printLn $ "Part One result: " ++ (show $ calculate nums syms)
    printLn $ "Part Two result: " ++ (show $ gearRatio nums syms)


main : IO ()
main = do
    run "aoc2023/day03/test.txt"
    run "aoc2023/day03/input.txt"
