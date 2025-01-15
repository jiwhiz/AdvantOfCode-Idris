module Main

import Data.Maybe
import Data.String
import Data.String.Parser
import Util

anyChar : Parser Char
anyChar = P $ \s =>
    if s.pos < s.maxPos then
        let ch = assert_total $ strIndex s.input s.pos in
        pure $ OK ch (S s.input (s.pos + 1) s.maxPos)
    else
        pure $ Fail s.pos "unexpected end of input"

Point : Type
Point = (Integer, Integer)

record Machine where
    constructor MkMachine
    a : Point
    b : Point
    p : Point

Show Machine where
    show m = "A:" ++ show m.a ++ " B:" ++ show m.b ++ " P:" ++ show m.p

parseButton : Parser Point
parseButton = do
    token "Button"
    skip $ anyChar
    token ": X+"
    x <- integer
    token ", Y+"
    y <- integer
    skip $ char '\n'
    pure (x, y)


parsePrize : Parser Point
parsePrize = do
    token "Prize: X="
    x <- integer
    token ", Y="
    y <- integer
    skip $ char '\n'
    pure (x, y)


parseMachine : Parser Machine
parseMachine = MkMachine <$> parseButton <*> parseButton <*> parsePrize <* many (char '\n')


winMachine : Integer -> Machine -> Maybe Integer
winMachine extra (MkMachine (ax, ay) (bx, by) (px, py)) =
    let
        px' = px + extra
        py' = py + extra
        b = (px' * ay - py' * ax) `div` (ay * bx - by * ax)
        a = (px' - bx * b) `div` ax
    in if (a * ax + b * bx == px' && a * ay + b * by == py') then Just (a * 3 + b) else Nothing


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (Right (machines, _)) := parse (some parseMachine) content | Left errMsg => printLn ("Parse error: " ++ errMsg)
        printLn $ "Part One result: " ++ (show $ foldl (+) 0 (map ((\m => fromMaybe 0 m ) . winMachine 0) machines))
        printLn $ "Part Two result: " ++ (show $ foldl (+) 0 (map ((\m => fromMaybe 0 m ) . winMachine 10000000000000) machines))


main : IO ()
main = do
    run "aoc2024/day13/test.txt"
    run "aoc2024/day13/input.txt"
