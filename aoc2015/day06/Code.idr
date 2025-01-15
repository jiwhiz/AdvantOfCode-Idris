module Main

import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.String.Extra
import Util

data CommType = ON | OFF | TOGGLE

record Command where
    constructor MkComm
    type : CommType
    startX : Int
    startY: Int
    endX : Int
    endY : Int


parseCommand : String -> Maybe Command
parseCommand str =
    if isPrefixOf "turn on" str then parseCoord ON (drop 8 str)
    else if isPrefixOf "turn off" str then parseCoord OFF (drop 9 str)
    else if isPrefixOf "toggle" str then parseCoord TOGGLE (drop 7 str)
    else Nothing
    where
        parseCoord : CommType -> String -> Maybe Command
        parseCoord type str = do
            let (sStr::_::eStr::_) := words str | _ => Nothing
            let (sx::sy::_) := forget $ split (==',') sStr | _ => Nothing
            let (ex::ey::_) := forget $ split (==',') eStr | _ => Nothing
            [| MkComm (Just type) (parsePositive sx) (parsePositive sy) (parsePositive ex) (parsePositive ey)|]


part1 : List String -> Nat
part1 lines =
    let commands = mapMaybe id $ (parseCommand <$> lines)
        result = runCommand commands SortedMap.empty
    in length $ filter id $ values result
    where
        runCommand : List Command -> SortedMap (Int, Int) Bool -> SortedMap (Int, Int) Bool
        runCommand [] map = map
        runCommand (c::rest) map = runCommand rest $
            foldl
            (\m, pos =>
                let v =
                    case lookup pos m of
                        Nothing => False
                        Just v' => v'
                in case c.type of
                    ON => insert pos True m
                    OFF => insert pos False m
                    TOGGLE => insert pos (not v) m
            )
            map
            [ (x, y) 
                | x <- [c.startX .. c.endX]
                , y <- [c.startY .. c.endY]
            ]


part2 : List String -> Nat
part2 lines =
    let commands = mapMaybe id $ (parseCommand <$> lines)
        result = runCommand commands SortedMap.empty
    in sum $ values result
    where
        runCommand : List Command -> SortedMap (Int, Int) Nat -> SortedMap (Int, Int) Nat
        runCommand [] map = map
        runCommand (c::rest) map = runCommand rest $
            foldl
            (\m, pos =>
                let v : Nat = case lookup pos m of
                        Nothing => Z
                        Just v' => v'
                    
                    turnOff : Nat -> Nat
                    turnOff Z = Z
                    turnOff (S k) = k

                in case c.type of
                    ON => insert pos (S v) m
                    OFF => insert pos (turnOff v) m
                    TOGGLE => insert pos (S (S v)) m
            )
            map
            [ (x, y) 
                | x <- [c.startX .. c.endX]
                , y <- [c.startY .. c.endY]
            ]


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        putStrLn $ "Part One result: " ++ (show $ part1 $ lines content)
        putStrLn $ "Part Two result: " ++ (show $ part2 $ lines content)


main : IO ()
main = do
    run "aoc2015/day06/test.txt"
    run "aoc2015/day06/input.txt"
