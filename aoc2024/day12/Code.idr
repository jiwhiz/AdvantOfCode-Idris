module Main

import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Data.SortedMap
import Decidable.Equality
import Util


data Dir = Up | Down | Left | Right

Eq Dir where
    Up == Up = True
    Down == Down = True
    Left == Left = True
    Right == Right = True
    _ == _ = False

Show Dir where
    show Up = "U"
    show Down = "D"
    show Left = "L"
    show Right = "R"


Ord Dir where
    compare a b = compare (show a) (show b)


Point : Type
Point = (Int, Int)


Grid : Type
Grid = SortedMap Point Char


loadData : String -> Grid
loadData text = go (unpack text) 0 0 empty
    where
        go : List Char -> Int -> Int -> Grid -> Grid
        go [] r c map = map
        go ('\n' :: cs) r c map = go cs (r + 1) 0 map
        go (x :: xs) r c map = go xs r (c + 1) $ insert (r,c) x map


step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)

dirList : List Dir
dirList = [Up, Down, Left, Right]


splitRegions : Grid -> List Grid
splitRegions map =
    case SortedMap.toList map of
        [] => []
        ((p, v)::_) => 
            let (map', region) = walk v (delete p map) (singleton p v) in
            region :: splitRegions map'
    where
        search : Point -> Char -> Grid -> List Point
        search pos value grid =
            foldl
                (\acc, dir =>
                    let next = step dir pos in
                    case lookup next grid of
                        Nothing => acc
                        Just v => if v == value then next :: acc else acc
                )
                []
                dirList

        walk : Char -> Grid -> Grid -> (Grid, Grid)
        walk value oldMap region =
            case foldl (\acc, p => acc ++ search p value oldMap) [] (keys region) of
                [] => (oldMap, region)
                ps => walk value (foldl (\m, p => delete p m) oldMap ps) (foldl (\r, p => insert p value r) region ps)
             

perimeter : Grid -> Int
perimeter grid =
    foldl (\acc, p => acc + (sum $ (\dir => check (step dir p)) <$> dirList)) 0 (keys grid)
    where
        check : Point -> Int
        check p =
            case lookup p grid of
                Nothing => 1
                Just _ => 0


part1 : Grid -> Int
part1 grid =
    foldl 
        (\acc, region => 
            let area = length $ SortedMap.toList region
                peri = perimeter region
            in acc +  (cast area * peri)
        )
        0
        (splitRegions grid)


sides : Grid -> Int
sides grid =
    foldl (\acc, p => acc + check p Up Right + check p Down Right + check p Left Down + check p Right Down) 0 (keys grid)
    where
        check : Point -> Dir -> Dir -> Int
        check p sideDir nextDir =
            let
                sideP = step sideDir p
                nextP = step nextDir p
                nextSideP = step sideDir nextP
            in
            case lookup sideP grid of
                Just _ => 0 -- connected, no side
                Nothing =>
                    case lookup nextP grid of
                        Nothing => 1 -- disconnected, side ends
                        Just _ => 
                            case lookup nextSideP grid of
                                Nothing => 0 -- along in one side, ignore
                                Just _ => 1 -- connected, side ends


part2 : Grid -> Int
part2 grid =
    foldl 
        (\acc, region => 
            let area = length $ SortedMap.toList region
                side = sides region
            in acc +  (cast area * side)
        )
        0
        (splitRegions grid)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := loadData content
        printLn $ "Part One result: " ++ (show $ part1 grid)
        printLn $ "Part Two result: " ++ (show $ part2 grid)


main : IO ()
main = do
    run "aoc2024/day12/test3.txt"
    run "aoc2024/day12/input.txt"
