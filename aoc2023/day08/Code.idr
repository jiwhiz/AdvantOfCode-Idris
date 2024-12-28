module Main

import Data.Fin
import Data.Fuel
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect
import Data.SortedMap
import Util


data Instr = LEFT | RIGHT

Show Instr where
    show i = case i of 
        LEFT => "L"
        RIGHT => "R"


parseInstrs : String -> List Instr
parseInstrs str = reverse $ parse (unpack str) []
    where
        parse : List Char -> List Instr -> List Instr
        parse [] acc = acc
        parse (c :: cs) acc =
            case c of
                'L' => parse cs (LEFT :: acc)
                'R' => parse cs (RIGHT :: acc)
                _   => parse cs acc


Node : Type
Node = Vect 3 Char

parseMap : List String -> SortedMap Node (Node, Node)
parseMap = doParse SortedMap.empty
    where 
        doParse : SortedMap Node (Node, Node) -> List String -> SortedMap Node (Node, Node)
        doParse networkMap [] = networkMap
        doParse networkMap (l::lines) =
            case unpack l of
                (n1 :: n2 :: n3 :: ' ' :: '=' :: ' ' :: '(' ::
                    l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: ')' :: _) =>
                    doParse (SortedMap.insert [n1, n2, n3] ([l1, l2, l3], [r1, r2, r3]) networkMap) lines
                _ => doParse networkMap lines


||| In the network map, take one step based on instruction from start node, return Nothing if cannot find start node
oneStep : SortedMap Node (Node, Node) -> Instr -> Node -> Maybe Node
oneStep networkMap instr start = do
    (left, right) <- lookup start networkMap
    case instr of
        LEFT => pure left
        RIGHT => pure right


part1 : Fuel -> List Instr -> SortedMap Node (Node, Node) -> Node -> Nat -> Nat
part1 (More fuel) instrs networkMap start preSteps =
    let
        Just (steps, node) := navigate instrs start | Nothing => 0
    in
        if (node == ['Z', 'Z', 'Z']) then preSteps + steps
        else part1 fuel instrs networkMap node (preSteps + steps)
    where
        navigate : List Instr -> Node -> Maybe (Nat, Node)
        navigate [] start = Just (0, start) -- reached the end of instructions
        navigate (x :: xs) start = do 
            ['Z', 'Z', 'Z'] <- oneStep networkMap x start
                | nextNode => do
                    (steps, endNode) <- navigate xs nextNode
                    pure (S steps, endNode)
            pure (1, ['Z', 'Z', 'Z'])
part1 Dry _ _ _ _ = 0


||| find nodes with specific ending letter
parseStartingNodes : Char -> List String -> List Node
parseStartingNodes endingChar = doParse [] where
    doParse : List Node -> List String -> List Node
    doParse acc [] = acc
    doParse acc (x :: xs) =
        case unpack x of
            a :: b :: c :: _ => if (c == endingChar) then doParse ([a, b, c] :: acc) xs else doParse acc xs
            _ => doParse acc xs


||| Map for a starting node (**A) to ending node (**Z) with steps
calculateRoundMap : List Instr -> SortedMap Node (Node, Node) -> List Node -> List Integer -- Maybe (SortedMap Node (Nat, Node))
calculateRoundMap instrs networkMap startNodes =
    fst <$> mapMaybe (naviWithFuel (limit 10000)) ((\x => (0, x)) <$> startNodes)
    where
        ||| navi one node to get steps to final node ended with Z
        naviWithFuel : Fuel -> (Integer, Node) -> Maybe (Integer, Node)
        naviWithFuel Dry _ = Nothing
        naviWithFuel (More fuel) (previousSteps, startNode) =
            case doNavi instrs startNode of
                Nothing => Nothing
                Just (steps, node@[_, _, 'Z']) => Just (steps + previousSteps, node)
                Just (steps, node) => naviWithFuel fuel (steps + previousSteps, node)
            where
                doNavi : List Instr -> Node -> Maybe (Integer, Node)
                doNavi [] start = Just (0, start) -- reach end of instr
                doNavi (instr :: is) start = 
                    case oneStep networkMap instr start of
                            Nothing => Nothing  -- stuck
                            Just z@[_, _, 'Z'] => Just (1, z)  -- found ending node
                            Just next => do
                                (s, end) <- doNavi is next
                                pure (s + 1, end)


part2 : List Instr -> SortedMap Node (Node, Node) -> List Node -> Integer
part2 instr2 networkMap startNodes =
    lcm $ calculateRoundMap instr2 networkMap startNodes


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let (instrLine :: _ :: mapLines) := lines content | _ => printLn "Not enough data"
        map := parseMap mapLines
        instrs := parseInstrs instrLine
        aNodes := parseStartingNodes 'A' mapLines

    putStrLn $ "Part One result: " ++ (show $ part1 (limit 100) instrs map ['A', 'A', 'A'] 0)
    putStrLn $ "Part Two result: " ++ (show $ part2 instrs map aNodes)


main : IO ()
main = do
    -- run "aoc2023/day08/test1.txt"
    -- run "aoc2023/day08/test2.txt"
    run "aoc2023/day08/input.txt"
