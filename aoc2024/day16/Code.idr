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

Ord Dir where
    compare a b = compare (show a) (show b)

dirList : List Dir
dirList = [Up, Down, Left, Right]


dirChar : Dir -> Char
dirChar Up = '^'
dirChar Down = 'v'
dirChar Left = '<'
dirChar Right = '>'


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

turnLeft : Dir -> Dir
turnLeft Up = Left
turnLeft Down = Right
turnLeft Left = Down
turnLeft Right = Up


turnRight : Dir -> Dir
turnRight Up = Right
turnRight Down = Left
turnRight Left = Up
turnRight Right = Down

turn : Dir -> Dir -> Int
turn Up Up = 0
turn Down Down = 0
turn Left Left = 0
turn Right Right = 0
turn Up Down = 2000
turn Down Up = 2000
turn Left Right = 2000
turn Right Left = 2000
turn _ _ = 1000

back : Dir -> Point -> Point
back Up (r, c) = (r+1, c)
back Down (r, c) = (r-1, c)
back Left (r, c) = (r, c+1)
back Right (r, c) = (r, c-1)


Queue : Type
Queue = SortedMap (Point, Dir) (Maybe Int)

find : Char -> Grid -> Maybe Point
find c grid =
    head' $ map fst $ filter ((== c) . snd ) $ SortedMap.toList grid


buildQueue : Grid -> Queue
buildQueue grid = build (toList grid) empty
    where
        fillDirs : Point -> Queue -> Queue
        fillDirs p q = insert (p, Up) Nothing (insert (p, Down) Nothing (insert (p, Left) Nothing (insert (p, Right) Nothing q)))
        
        update : Char -> Point -> Queue -> Queue
        update '.' p q = fillDirs p q
        update 'E' p q = fillDirs p q
        update 'S' p q = insert (p, Left) (Just 0) q
        update _ p q = q

        build : List (Point, Char) -> Queue -> Queue
        build [] q = q
        build ((p, c):: rest) q = build rest (update c p q)

DirMap : Type
DirMap = SortedMap (Point, Dir) (Int, Point, Dir)

findMin : Queue -> Maybe (Int, Point, Dir)
findMin q =
    let list = SortedMap.toList q
        list' = mapMaybe id $ map 
            (\((p, d), v) => case v of
                Nothing => Nothing
                Just s => Just (s, p, d)
            )
            list
        ordered = sortBy (\(s1, _, _), (s2, _, _) => compare s1 s2) list'
        (l::ls) = ordered | _ => Nothing
    in Just l


travel : Point -> Queue -> SortedMap (Point, Dir) Int -> (Queue, SortedMap (Point, Dir) Int)
travel end queue map =
    case findMin queue of
        Nothing => (queue, map) -- end of travel
        Just (score, pos, dir) =>
            if pos == end then
                travel end (delete (pos, dir) queue) map -- continue to find all optimal paths
            else 
                let (queue', map') =
                    go (pos, dir) (turnLeft $ turnLeft dir) (score + 2001) $
                    go (pos, dir) (turnLeft dir) (score + 1001) $
                    go (pos, dir) (turnRight dir) (score + 1001) $
                    go (pos, dir) dir (score + 1) ((delete (pos, dir) queue), map)
                in
                travel end queue' map'
                --(queue', map')

    where
        go : (Point, Dir) -> Dir -> Int -> (Queue, SortedMap (Point, Dir) Int) -> (Queue, SortedMap (Point, Dir) Int)
        go (previous, dir) d s (q, m) =
            let next = step d previous in
            case lookup (next, d) q of
                Just Nothing => (insert (next, d) (Just s) q, insert (next, d) s m)
                Just (Just s') =>
                    if (s < s') then
                        ((insert (next, d) (Just s) q), insert (next, d) s m)
                    else (q, m)
                _ => (q, m) 


lookup : (Point, Dir) -> SortedMap (Point, Dir) Int -> Maybe ((Point, Dir), Int)
lookup dp map =
    case SortedMap.lookup dp map of
        Nothing => Nothing
        Just v => Just (dp, v)


part2 : SortedMap (Point, Dir) Int -> List ((Point, Dir), Int) -> Grid -> Grid
part2 scores [] tiles = tiles
part2 scores (((p, d), cost) :: rest) tiles =
    let tiles' = insert p 'O' tiles
        backwards = mapMaybe (flip lookup scores) $ map (\dir => (back d p, dir)) dirList
        next = filter valid backwards
    in
    part2 scores (next ++ rest) tiles'
    where
        valid : ((Point, Dir), Int) -> Bool
        valid cand@((pt', dir'), cost') = cost == cost' + 1 + turn d dir'


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := parseGrid $ lines content
        let Just start := find 'S' grid | _ => printLn "No start point"
        let Just end := find 'E' grid | _ => printLn "No end point"

        let queue := buildQueue grid
        let (queue', map') := travel end queue (singleton (start, Left) 0)

        let (ends@((_, e) :: es)) := filter (\((p, d), _) => p == end ) $ SortedMap.toList map' | _ => printLn "No end found"
        let optimalScore := foldl min e $ map snd es
        printLn $ "Part One result: " ++ (show optimalScore)
        
        let minEnds := filter (\(_, score) => score == optimalScore ) ends
        let tileGrid := part2 map' minEnds empty

        printLn $ "Part Two result: " ++ (show $ length $ SortedMap.toList tileGrid)


main : IO ()
main = do
    run "aoc2024/day16/test1.txt"
    run "aoc2024/day16/test2.txt"
    run "aoc2024/day16/input.txt"
