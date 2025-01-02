module Main

import Data.List
import Data.String
import Data.SortedMap
import Util


Point : Type
Point = (Integer, Integer)


Grid : Type
Grid = SortedMap Point Char


data Dir = Up | Down | Left | Right

Eq Dir where
    Up == Up = True
    Down == Down = True
    Left == Left = True
    Right == Right = True
    _ == _ = False

Show Dir where
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"

Ord Dir where
    compare a b = compare (show a) (show b)


step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)


dirList : List Dir
dirList = [Up, Down, Left, Right]


parseDir : String -> Dir
parseDir "R" = Right
parseDir "L" = Left
parseDir "U" = Up
parseDir "D" = Down
parseDir _ = Up


walk : List (Dir, Integer) -> Point -> Grid -> (Grid, Point)
walk [] pos grid = (grid, pos)
walk ((dir, n)::xs) pos grid =
    let (grid', pos') =
        foldl
            (\(g, p), _ =>
                let next = step dir p in
                (insert next '#' g, next)
            )
            (grid, pos)
            [1..n]
    in walk xs pos' grid'


fill : (Point, Point) -> Grid -> Grid
fill ((minRow, minCol), (maxRow, maxCol)) grid =
    foldl fillSpace grid
        (  [(x, minCol - 1) | x <- [minRow..maxRow]]
        ++ [(x, maxCol + 1) | x <- [minRow..maxRow]]
        ++ [(minRow - 1, y) | y <- [minCol..maxCol]]
        ++ [(maxRow + 1, y) | y <- [minCol..maxCol]]
        )
    where
        fillSpace : Grid -> Point -> Grid
        fillSpace grid pos =
            foldl
                (\g, dir =>
                    let next = step dir pos in
                    if inGrid next then
                        case lookup next g of
                            Nothing => fillSpace (insert next '.' g) next
                            _ => g
                    else g
                )
                grid
                dirList
            where
                inGrid : Point -> Bool
                inGrid (row, col) =
                    (row >= minRow) && (row <= maxRow) && (col >= minCol) && (col <= maxCol)


dig : (Point, Point) -> Grid -> Grid
dig ((minRow, minCol), (maxRow, maxCol)) grid =
    foldl
        (\g, pos =>
            case lookup pos g of
                Nothing => insert pos '#' g
                _ => g
        )
        grid
        [ (x, y) | x <- [minRow..maxRow], y <- [minCol..maxCol]]


calculate : (Point, Point) -> Grid -> Integer
calculate ((minRow, minCol), (maxRow, maxCol)) grid =
    foldl
        (\acc, pos =>
            case lookup pos grid of
                Nothing => acc + 1
                Just '#' => acc + 1
                _ => acc
        )
        0
        [ (x, y) | x <- [minRow..maxRow], y <- [minCol..maxCol]]


part1 : List String -> Integer
part1 lines =
    let
        plan = parsePlan lines
        (grid, end) = walk plan (0, 0) empty
        Just minRow = head' $ sort $ map fst $ keys grid | _ => 0
        Just maxRow = head' $ reverse $ sort $ map fst $ keys grid | _ => 0
        Just minCol = head' $ sort $ map snd $ keys grid | _ => 0
        Just maxCol = head' $ reverse $ sort $ map snd $ keys grid | _ => 0
        area = ((minRow, minCol), (maxRow, maxCol))
        gridAfterFill = fill area grid
    in calculate area gridAfterFill
    where
        parsePlan : List String -> List (Dir, Integer)
        parsePlan [] = []
        parsePlan (x::xs) = 
            let (dirStr::stepStr::_) = words x | _ => []
                Just step = parseInteger stepStr | _ => []
            in (parseDir dirStr, step) :: parsePlan xs


hexDigitToInt : Char -> Maybe Int
hexDigitToInt c =
  if '0' <= c && c <= '9' then Just $ ord c - ord '0'
  else if 'A' <= c && c <= 'F' then Just $ ord c - ord 'A' + 10
  else if 'a' <= c && c <= 'f' then Just $ ord c - ord 'a' + 10
  else Nothing

parseHex : List Char -> Maybe Int
parseHex hexDigits =
    foldlM
        (\acc, x => do
            x' <- hexDigitToInt x
            pure (acc * 16 + x')
        )
        0
        hexDigits


record VLine where
    constructor MkVLine
    column : Integer
    top : Integer
    bottom : Integer 

Show VLine where
    show vl = "VLine(col:" ++ show vl.column ++ ", row from " ++ show vl.top ++ " to " ++ show vl.bottom ++ ")"


mutual
    ||| Scan from outside lagoon, row can touch l1 and l2 at top or bottom or middle, but cannot touch l1 at top/bottom and l2 in the middle
    scan1 : Integer -> List VLine -> Integer
    scan1 row (l1@(MkVLine col1 top1 bottom1)::l2@(MkVLine col2 top2 bottom2)::rest) =
        if (row == top1 && row == top2) || (row == bottom1 && row == bottom2) then (col2 - col1 + 1) + scan1 row rest -- single horizontal line, count length with boundary then continue scan rest
        else if (row == top1 && row == bottom2) || (row == bottom1 && row == top2) then (col2 - col1) + scan2 row (l2::rest) -- corner and enter into lagoon, use scan2
        else if (row == top2 || row == bottom2) then (col2 - col1) + scan3 row (l2::rest) -- from middle to horizontal line, use scan3
        else (col2 - col1 + 1) + scan1 row rest
    scan1 _ _ = 0 -- return when 0 or 1 line left

    ||| Scan after a horizontal line, and inside lagoon, so l1 must be top or bottom
    scan2 : Integer -> List VLine -> Integer
    scan2 row (l1@(MkVLine col1 top1 bottom1)::l2@(MkVLine col2 top2 bottom2)::rest) =
        if (row > top2 && row < bottom2) then (col2 - col1 + 1) + scan1 row rest -- reach l2 middle, continue scan rest
        else (col2 - col1) + scan3 row (l2::rest)  -- reach another horizontal line
    scan2 _ _ = 0 -- return when 0 or 1 line left

    ||| From inside lagoon scan a horizontal line, row touch l1 and l2 with pair of top or bottom
    scan3 : Integer -> List VLine -> Integer
    scan3 row (l1@(MkVLine col1 top1 bottom1)::l2@(MkVLine col2 top2 bottom2)::rest) =
        if (row == top1 && row == top2) || (row == bottom1 && row == bottom2) then (col2 - col1) + scan2 row (l2::rest)  -- continue scan inside lagoon
        else (col2 - col1 + 1) + scan1 row (rest)  -- out of lagoon, scan rest use scan1
    scan3 _ _ = 0 -- return when 0 or 1 line left


part2 : List String -> Integer 
part2 strList =
    let plan = parsePlan strList
        (end, vlist) = walkLines plan (0, 0) []
        vlines = sortBy (\(MkVLine col1 top1 bottom1), (MkVLine col2 top2 bottom2) => compare col1 col2) vlist
        Just minRow := head' $ sort $ map (\vl => vl.top ) $ vlines | _ => 0
        Just maxRow := head' $ reverse $ sort $ map (\vl => vl.bottom ) $ vlines | _ => 0
    in foldl 
        (\acc, row => 
            acc + scan1 row (cross row vlines)
        )
        0
        [minRow..maxRow]
    where
        parsePlan : List String -> List (Dir, Integer)
        parsePlan [] = []
        parsePlan (x::xs) = 
            let (_::_::str::_) = words x | _ => []
                chars : List Char = unpack str
                Just step = parseHex $ take 5 $ drop 2 chars | _ => []
                Just dirChar = head' $ drop 7 chars | _ => []
                dir = case dirChar of '0'=>Right; '1'=>Down; '2'=>Left; '3'=>Up; _=>Up
            in (dir, cast step) :: parsePlan xs

        walkLines : List (Dir, Integer) -> Point -> List VLine -> (Point, List VLine)
        walkLines [] pos vlist = (pos, vlist)
        walkLines ((dir, steps)::xs) (row, col) vlist =
            case dir of
                Up => walkLines xs (row - steps, col) ( (MkVLine col (row - steps) row) :: vlist)
                Down => walkLines xs (row + steps, col) ( (MkVLine col row (row + steps)) :: vlist)
                Left => walkLines xs (row, col - steps) vlist
                Right => walkLines xs (row, col + steps) vlist

        cross : Integer -> List VLine -> List VLine
        cross row [] = []
        cross row (x::xs) = if row >= x.top && row <= x.bottom then x :: cross row xs else cross row xs


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ part1 $ lines content)
        putStrLn $ "Part Two result: " ++ (show $ part2 $ lines content)


main : IO ()
main = do
    run "aoc2023/day18/test.txt"
    run "aoc2023/day18/input.txt"
