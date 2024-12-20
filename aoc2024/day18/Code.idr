module Main


import Data.Fin
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
Queue = SortedMap Point (Maybe Int)


DirMap : Type
DirMap = SortedMap (Point, Dir) (Int, Point, Dir)

findMin : Queue -> Maybe (Int, Point)
findMin q =
    let list = mapMaybe id $ map
            (\(p, v) => case v of
                Nothing => Nothing
                Just s => Just (s, p)
            )
            $ SortedMap.toList q
        ordered = sortBy (\(s1, _), (s2, _) => compare s1 s2) list
        (l::ls) = ordered | _ => Nothing
    in Just l


travel : Point -> Queue -> SortedMap Point Int -> (Queue, SortedMap Point Int)
travel end queue map =
    case findMin queue of
        Nothing => (queue, map) -- end of travel
        Just (score, pos) =>
            if pos == end then
                (queue, map) -- terminate
            else 
                let (queue', map') =
                    go pos (step Up pos) (score + 1) $
                    go pos (step Down pos) (score + 1) $
                    go pos (step Left pos) (score + 1) $
                    go pos (step Right pos) (score + 1) ((delete pos queue), map)
                in
                travel end queue' map'

    where
        go : Point -> Point -> Int -> (Queue, SortedMap Point Int) -> (Queue, SortedMap Point Int)
        go previous next s (q, m) =
            case lookup next q of
                Just Nothing => (insert next (Just s) q, insert next s m)
                Just (Just s') =>
                    if (s < s') then
                        (insert next (Just s) q, insert next s m)
                    else (q, m)
                _ => (q, m) 



parseInput : List String -> List Point
parseInput [] = []
parseInput (l::lines) = 
    let (x::y::_) = forget $ split (== ',') l | _ => []
        Just px = parseInteger x | Nothing => []
        Just py = parseInteger y | Nothing => []
    in (py, px) :: parseInput lines


buildQueue : List (Point, Char) -> Queue -> Queue
buildQueue [] queue = queue
buildQueue ((p, c) :: rest) queue =
    buildQueue rest $ if c == '.' then insert p Nothing queue else queue


buildGrid : Nat -> List Point -> Grid -> Grid
buildGrid Z _ grid = grid
buildGrid (S k) [] grid = grid
buildGrid (S k) (p::ps) grid = buildGrid k ps (insert p '#' grid)

findPath : Int -> Nat -> List Point -> Maybe Int 
findPath size count positions =
    let grid = buildGrid count positions $ fromList $ (\pos => (pos, '.')) <$> [(x, y) | x <- [0..size-1], y <- [0..size-1]]
        queue = insert (0, 0) (Just 0) $ buildQueue (toList grid) empty
        end = (size-1, size-1)
        (queue', map') = travel end queue (singleton (0,0) 0)
        (ends@((_, e) :: es)) := filter (\(p, _) => p == end ) $ SortedMap.toList map' | _ => Nothing
    in Just e



part2 : Int -> List Point -> Maybe Point
part2 gridSize positions = search 0 (cast (length positions) - 1)
    where
        search : Int -> Int -> Maybe Point
        search left right =
            if left + 1 == right then
                let idx : Nat = cast right in
                case inBounds idx positions of
                     (Yes prf) => Just $ index idx positions
                     (No contra) => Nothing
                
            else
                let idx : Nat = divNatNZ (cast left + cast right) 2 SIsNonZero
                in case findPath gridSize (S idx) positions of
                    Nothing => search left (cast idx)
                    Just _ => search (cast idx) right


run : Int -> Nat -> String -> IO ()
run size count filename = do
    printLn filename
    content <- Util.loadFile filename
    
    let positions := parseInput $ lines content
    let optimalScore := findPath size count positions
    let Just coordinates := part2 size positions | _ => printLn "No result for part 2"
    printLn $ "Part 1 : " ++ (show optimalScore)
    printLn $ "Part 2 : " ++ (show $ snd coordinates) ++ "," ++ (show $ fst coordinates)


main : IO ()
main = do
    run 7 12 "aoc2024/day18/test.txt"
    run 71 1024 "aoc2024/day18/input.txt"

