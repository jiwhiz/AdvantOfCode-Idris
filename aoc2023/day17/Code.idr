module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
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
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"

Ord Dir where
    compare a b = compare (show a) (show b)


parseDigit : Char -> Int
parseDigit '1' = 1
parseDigit '2' = 2
parseDigit '3' = 3
parseDigit '4' = 4
parseDigit '5' = 5
parseDigit '6' = 6
parseDigit '7' = 7
parseDigit '8' = 8
parseDigit '9' = 9
parseDigit _ = 0


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


stepN : Int -> Dir -> Point -> Point
stepN 0 dir p = p
stepN n dir p = stepN (n-1) dir (step dir p)


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


-- Priority Queue using SortedSet, store: distance (heat loss), position, direction
Queue : Type
Queue = SortedSet (Int, Point, Dir)


-- Distance Map to store min distance (heat loss) for every node (with direction) searched so far
DistMap : Type
DistMap = SortedMap (Point, Dir) Int


-- Dijkstra Algorithm
dijkstra : Grid -> Point -> Int -> Int -> Queue -> DistMap -> Maybe Int
dijkstra grid target low high queue distMap =
    case head' $ SortedSet.toList queue of
        Nothing => Nothing  -- no result
        Just (dist, current, dir) =>
            if target == current then -- reach the target, return optimal heat loss
                --(dist, newPath) :: dijkstra grid target (delete (dist, current, dir, path) queue) distMap 
                Just dist
            else
                let
                    adj = neighbors current dir
                    updatedQueue = foldl (updateQueue dist distMap) (delete (dist, current, dir) queue) adj
                    updatedDistances = updateDistances dist distMap adj
                in
                    dijkstra grid target low high updatedQueue updatedDistances
    where
        -- Get neighbors of a state (position with direction and heat loss inc), find all possible next neighbors by turn left/right then step in range
        neighbors : Point -> Dir -> List (Point, Dir, Int)
        neighbors pos dir = walk (turnLeft dir) ++ walk (turnRight dir)
            where
                walk : Dir -> List (Point, Dir, Int)
                walk newDir = fst $ foldl
                    (\(accList, accLoss), steps =>
                        let next = stepN steps newDir pos in
                            case lookup next grid of
                                Just c => 
                                    if steps < low then (accList, accLoss + (parseDigit c))
                                    else ((next, newDir, accLoss + (parseDigit c)) :: accList, accLoss + (parseDigit c))
                                _ => (accList, accLoss)
                    )
                    ([], 0)
                    [1..high]


        -- Update the priority queue with neighbors
        updateQueue : Int -> DistMap -> Queue -> (Point, Dir, Int) -> Queue
        updateQueue currentDist distMap queue (neighbor, dir, inc) =
            let newDist = currentDist + inc in
            case lookup (neighbor, dir) distMap of
                Nothing => insert (newDist, neighbor, dir) queue  -- first time visit the position, add to queue
                Just oldDist => if newDist <= oldDist then
                                    insert (newDist, neighbor, dir) queue  -- optimal path, add to queue with this new path
                                else queue  -- not optimal, ignore this path

        -- Update distances map
        updateDistances : Int -> DistMap -> List (Point, Dir, Int) -> DistMap
        updateDistances currentDist distMap [] = distMap
        updateDistances currentDist distMap ((neighbor, dir, inc) :: rest) =
            let newDist = currentDist + inc in
            case lookup (neighbor, dir) distMap of
                Nothing => updateDistances currentDist (insert (neighbor, dir) newDist distMap) rest
                Just oldDist => if newDist < oldDist
                                then updateDistances currentDist (insert (neighbor, dir) newDist distMap) rest
                                else updateDistances currentDist distMap rest


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let grid := parseGrid $ lines content
        let Just start := Builtin.fst <$> (head' $ toList grid) | _ => printLn "No grid data"
        let Just end := Builtin.fst <$> (head' $ reverse $ toList grid) | _ => printLn "No grid data"
        let initialQueue : Queue = insert (0, start, Down) $ singleton (0, start, Right)
        putStrLn $ "Part One result: " ++ (show $ dijkstra grid end 1 3 initialQueue SortedMap.empty)
        putStrLn $ "Part Two result: " ++ (show $ dijkstra grid end 4 10 initialQueue SortedMap.empty)


main : IO ()
main = do
    run "aoc2023/day17/test.txt"
    run "aoc2023/day17/input.txt"
