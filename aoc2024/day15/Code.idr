module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.String
import Util
import Debug.Trace

Point : Type
Point = (Int, Int)


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
    show Up = "Up"
    show Down = "Down"
    show Left = "Left"
    show Right = "Right"

combine : SortedMap Int Integer -> List (Int, Integer) -> SortedMap Int Integer
combine map [] = map
combine map ((k, v)::xs) =
    case lookup k map of
        Nothing => combine (insert k v map) xs 
        Just v' => combine (insert k (v + v') map) xs

parseGrid : List String -> Grid
parseGrid lines = go 0 lines empty
    where
        parseRow : Int -> Int -> List Char -> Grid -> Grid
        parseRow _ _ [] grid = grid
        parseRow row col (c::cs) grid = parseRow row (col + 1) cs (insert (row, col) c grid)

        go : Int -> List String -> Grid -> Grid
        go _ [] grid = grid
        go row (x::xs) grid = go (row + 1) xs (parseRow row 0 (unpack x) grid)
            

step : Dir -> Point -> Point
step Up (r, c) = (r-1, c)
step Down (r, c) = (r+1, c)
step Left (r, c) = (r, c-1)
step Right (r, c) = (r, c+1)

findRobot : Grid -> Maybe Point
findRobot grid =
    head' $ map fst $ filter ((== '@') . snd ) $ SortedMap.toList grid


move : Grid -> Dir -> Grid
move grid dir =
    let Just robot = findRobot grid | Nothing => grid
        next = step dir robot
    in case lookup next grid of
        Just '.' => insert next '@' (insert robot '.' grid)
        Just 'O' =>
            case findNextSpace next dir of
                Nothing => grid
                Just pos => insert pos 'O' (insert next '@' (insert robot '.' grid))
        _ => grid
    where
        findNextSpace : Point -> Dir -> Maybe Point
        findNextSpace pos dir =
            let next = step dir pos in
            case lookup next grid of
                Just 'O' => findNextSpace next dir
                Just '.' => Just next
                _ => Nothing


proceed : List Dir -> Grid -> Grid
proceed [] grid = grid
proceed (x::xs) grid = proceed xs (move grid x)


coordinates : Char -> Grid -> Int
coordinates char grid = go $ toList grid
    where
        go : List (Point, Char) -> Int
        go [] = 0
        go (((row, col), c)::xs) = if c == char then (row*100+col) + go xs else go xs

parseGrid2 : List String -> Grid
parseGrid2 lines = go 0 lines empty
    where
        parseRow : Int -> Int -> List Char -> Grid -> Grid
        parseRow _ _ [] grid = grid
        parseRow row col (c::cs) grid =
            let grid' = case c of
                    '#' => insert (row, col * 2) '#' (insert (row, col*2+1) '#' grid)
                    '.' => insert (row, col * 2) '.' (insert (row, col*2+1) '.' grid)
                    'O' => insert (row, col * 2) '[' (insert (row, col*2+1) ']' grid)
                    '@' => insert (row, col * 2) '@' (insert (row, col*2+1) '.' grid)
                    _ => grid
            in parseRow row (col + 1) cs grid'

        go : Int -> List String -> Grid -> Grid
        go _ [] grid = grid
        go row (x::xs) grid = go (row + 1) xs (parseRow row 0 (unpack x) grid)


move2 : Grid -> Dir -> Grid
move2 grid dir =
    let Just robot = findRobot grid | Nothing => grid
        next = step dir robot
    in case lookup next grid of
        Just '.' => insert next '@' (insert robot '.' grid)
        Just '[' => case dir of
            Right => hPush Right '[' grid next ((insert next '@' (insert robot '.' grid)))
            Left => hPush Left '[' grid next ((insert next '@' (insert robot '.' grid)))
            Up => vPush Up grid [next] (insert (step Right next) '.' (insert next '@' (insert robot '.' grid)))
            Down => vPush Down grid [next] (insert (step Right next) '.' (insert next '@' (insert robot '.' grid)))
        Just ']' => case dir of
            Right => hPush Right ']' grid next ((insert next '@' (insert robot '.' grid)))
            Left => hPush Left ']' grid next ((insert next '@' (insert robot '.' grid)))
            Up => vPush Up grid [step Left next] (insert (step Left next) '.' (insert next '@' (insert robot '.' grid)))
            Down => vPush Down grid [step Left next] (insert (step Left next) '.' (insert next '@' (insert robot '.' grid)))
        _ => grid

    where
        getBoxPositions : Char -> Point -> List (Point, Char)
        getBoxPositions c pos =
            if c == '[' then (pos, c) :: [(step Right pos, ']')]
            else (pos, c) :: [(step Left pos, '[')]
        
        hPush : Dir -> Char -> Grid -> Point -> Grid -> Grid
        hPush dir c original pos grid =
            let next = step dir pos in
            case lookup next grid of
                Nothing => original
                Just '#' => original
                Just '.' => insert next c grid
                Just c' => hPush dir c' original next $ insert next c grid

        vPush : Dir -> Grid -> List Point -> Grid -> Grid
        vPush dir original posList grid =
            let (afterGrid, afterList, failed) : (Grid, List Point, Bool) =
                foldl
                    (\(g, list, terminate), pos => 
                        if terminate then (g, list, terminate)
                        else let next = step dir pos in
                            case (SortedMap.lookup next g, SortedMap.lookup (step Right next) g) of
                                (Just '[', Just ']') => (g, next::list, False)
                                (Just '.', Just '.') => (insert next '[' (insert (step Right next) ']' g), list, False)
                                (Just ']', Just '.') => (insert next '[' (insert (step Right next) ']' (insert (step Left next) '.' g)), (step Left next)::list, False)
                                (Just '.', Just '[') => (insert next '[' (insert (step Right next) ']' (insert (step Right $ step Right next) '.' g)), (step Right next)::list, False)
                                (Just ']', Just '[') => (insert next '[' (insert (step Right next) ']' (insert (step Right $ step Right next) '.' (insert (step Left next) '.' g))), (step Left next):: (step Right next)::list, False)
                                _ => (g, list, True)

                    )
                    (grid, [], False)
                    posList
            in
            if failed then original
            else case afterList of
                [] => afterGrid
                _ => vPush dir original afterList afterGrid



proceed2 : List Dir -> Grid -> Grid
proceed2 [] grid = grid
proceed2 (x::xs) grid = proceed2 xs (move2 grid x)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (p1, p2) := break (=="") $ lines content
        let warehouse := parseGrid p1
        let moves := (\c => if c == '^' then Up else if c == 'v' then Down else if c == '<' then Left else Right) <$> (unpack $ joinBy "" p2)
        let after := proceed moves warehouse
        printLn $ "Part One result: " ++ show (coordinates 'O' after)

        let warehouse2 := parseGrid2 p1
        let after2 := proceed2 moves warehouse2
        printLn $ "Part Two result: " ++ show (coordinates '[' after2)


main : IO ()
main = do
    run "aoc2024/day15/test1.txt"
    run "aoc2024/day15/test2.txt"
    run "aoc2024/day15/test3.txt"
    run "aoc2024/day15/input.txt"
