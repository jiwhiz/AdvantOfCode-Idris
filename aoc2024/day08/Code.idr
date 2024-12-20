module Main

import Data.Fin
import Data.List
import Data.List1
import Data.String
import Data.Vect
import Data.SortedMap
import Decidable.Equality
import Util


-- Define Board and Cell data types


Point : Type
Point = (Int, Int)


Grid : Type
Grid = SortedMap Point Bool


AntennaMap : Type
AntennaMap = SortedMap Char (List Point)

loadData : String -> (AntennaMap, Grid)
loadData text = go (unpack text) 0 0 (empty, empty)
  where
    go : List Char -> Int -> Int -> (AntennaMap, Grid) -> (AntennaMap, Grid)
    go [] r c maps = maps
    go ('\n' :: cs) r c maps = go cs (r + 1) 0 maps
    go ('.' :: cs) r c (map, grid) = go cs r (c + 1) (map, insert (r, c) False grid)
    go (x :: xs) r c (map, grid) = go xs r (c + 1) (merge map (SortedMap.singleton x [(r, c)]), insert (r, c) False grid)


part1 : AntennaMap -> Grid -> Int
part1 map grid =
    sum $ (\v => if v then 1 else 0) <$> (values $ foldl findAntinodes grid (values map))
    --foldl findAntinodes grid (values map)
    where
        create : Grid -> Point -> List Point -> Grid
        create grid _ [] = grid
        create grid p (q::xs) = 
            let pos1 = (2 * (fst p) - (fst q), 2 * (snd p) - (snd q))
                pos2 = (2 * (fst q) - (fst p), 2 * (snd q) - (snd p))
                grid1 = case lookup pos1 grid of
                        Nothing => grid
                        Just _ => insert pos1 True grid
                grid2 = case lookup pos2 grid1 of
                        Nothing => grid1
                        Just _ => insert pos2 True grid1
            in create grid2 p xs

        findAntinodes : Grid -> List Point -> Grid
        findAntinodes grid [] = grid
        findAntinodes grid (x :: xs) = findAntinodes (create grid x xs) xs
        

part2 : AntennaMap -> Grid -> Int
part2 map grid =
    sum $ (\v => if v then 1 else 0) <$> (values $ foldl findAntinodes grid (values map))
    where
        extend : Point -> Point -> Grid -> Grid
        extend pos delta grid =
            let pos' = (fst pos + fst delta, snd pos + snd delta)
                grid' = insert pos True grid
            in case lookup pos' grid of
                Nothing => grid'
                Just _ => extend pos' delta (insert pos' True grid')

        scan : Point -> List Point -> Grid -> Grid
        scan _ [] grid = grid
        scan p (q::xs) grid = 
            let grid1 = extend p (fst p - fst q, snd p - snd q) grid
                grid2 = extend q (fst q - fst p, snd q - snd p) grid1
            in scan p xs grid2

        findAntinodes : Grid -> List Point -> Grid
        findAntinodes grid [] = grid
        findAntinodes grid (x :: xs) = findAntinodes (scan x xs grid) xs


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let (map, grid) := loadData content

        printLn $ "Part One result: " ++ show (part1 map grid)
        printLn $ "Part Two result: " ++ show (part2 map grid)


main : IO ()
main = do
    run "aoc2024/day08/test.txt"
    run "aoc2024/day08/input.txt"
