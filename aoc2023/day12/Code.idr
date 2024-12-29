module Main

import Debug.Trace
import Data.Fin
import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.Vect
import Decidable.Equality
import Control.Monad.State
import Util

data Code = O | D | U

Show Code where
    show c = case c of O => "."; D => "#"; U => "?"


parseCode : Char -> Code
parseCode c = case c of '.' => O; '#' => D; _ => U 

parseNumbers : String -> List Nat
parseNumbers str = reverse $ 
    foldl
        (\acc, elm =>
            case parsePositive elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (forget $ split (',' ==) str)

parseLine : String -> (List Code, List Nat)
parseLine str = bimap (\codes => parseCode <$> unpack codes) (parseNumbers) $ break isSpace str


mutual
    count
        : {l, m : Nat}
        -> (codes : Vect l Code)
        -> (groups : Vect m Nat)
        -> (acc : Nat)
        -> State (SortedMap (Nat, Nat) Nat) Nat
    count [] [] Z = pure 1  -- reach end and no mismatched damaged springs, count as 1
    count [] [] (S _) = pure 0  -- reach end but have mismatched damaged spring, no count
    count [] (x::xs) acc = if acc == x then count [] xs Z else pure 0 
    count (x :: xs) [] Z =
        case x of
            D => pure 0
            _ => count xs [] Z
    count (x :: xs) [] (S _) = pure 0
    count codes@(x :: xs) groups@(y :: ys) acc =
        case x of
            O => if (acc == 0) then checkCache xs groups
                 else if (acc == y) then checkCache xs ys
                 else pure 0
            D => if (acc < y) then count xs groups (S acc)
                 else pure 0
            U => do
                resultD <- count (D::xs) groups acc
                resultO <- count (O::xs) groups acc
                pure (resultD + resultO)

    checkCache
         : {l, m : Nat}
        -> (codes : Vect l Code)
        -> (groups : Vect m Nat)
        -> State (SortedMap (Nat, Nat) Nat) Nat
    checkCache codes groups = do 
        memo <- get
        case SortedMap.lookup (l, m) memo of
            Nothing => do
                result <- count codes groups Z -- count from 
                modify (insert (l, m) result)
                pure result
            Just result => pure result


calculate : List (List Code, List Nat) -> Nat
calculate lines =
    foldl 
        (\acc, (codes, numbers) => acc + evalState SortedMap.empty (count (fromList codes) (fromList numbers) Z))
        0
        lines


unfoldCode : List Code -> List Code
unfoldCode x = concatMap id $ List.intersperse [U] $ List.replicate 5 x

unfoldNum : List Nat -> List Nat
unfoldNum x = concatMap id $ List.replicate 5 x


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let records := parseLine <$> lines content

    putStrLn $ "Part One result: " ++ (show $ calculate records)
    putStrLn $ "Part Two result: " ++ (show $ calculate $ (bimap unfoldCode unfoldNum <$> records))


main : IO ()
main = do
    run "aoc2023/day12/test.txt"
    run "aoc2023/day12/input.txt"
