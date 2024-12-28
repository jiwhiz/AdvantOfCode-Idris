module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Util

data CubeColor = Red | Green | Blue

record HandfulCubes where
    constructor MkHandfulCubes
    red : Integer
    green : Integer
    blue : Integer

parseColor : String -> Maybe CubeColor
parseColor str =
    case toLower str of
        "red" => Just Red
        "green" => Just Green
        "blue" => Just Blue
        _ => Nothing

parseId : String -> Maybe Integer
parseId str =
    parseInteger $ snd $ break isSpace $ fst $ break (== ':') str

||| parse string like `3 red` and return cube color with number of cubes
parseCubes : String -> Maybe (CubeColor, Integer) 
parseCubes str =
    let (numStr, colorStr) = break (isSpace) str
    in do
        color <- parseColor $ trim colorStr
        num <- parseInteger $ trim numStr
        pure (color, num)

parseHandful : String -> Maybe HandfulCubes
parseHandful handfulStr =
    foldlM
        (\handful, str => do 
            (color, num) <- parseCubes $ trim str
            case color of
                Red => pure $ { red := num } handful
                Green => pure $ { green := num } handful
                Blue => pure $ { blue := num } handful
        )
        (MkHandfulCubes 0 0 0)
        (forget $ split (==',') $ trim handfulStr)

getHandfulsLine : String -> Maybe String
getHandfulsLine line =
    head' $ tail $ split (==':') line


-- -------------------------
-- PART I

limit : HandfulCubes
limit = (MkHandfulCubes 12 13 14)

||| parse one game line and return game id if cubes are not over limit
parseOneGame : String -> Maybe Integer
parseOneGame line =
    do
        lineId <- parseId line
        handfuls <- (getHandfulsLine line)
        valid <- foldlM
            (\v, str => do
                h <- parseHandful str
                if (h.red > limit.red || h.green > limit.green || h.blue > limit.blue) then Nothing else Just v
            )
            True
            (forget $ split (== ';') $ handfuls)
        if valid then pure lineId else Nothing

partOne : List String -> Integer
partOne [] = 0
partOne (l::ls) =
    (fromMaybe 0 $ parseOneGame l) + partOne ls

-- -------------------------
-- PART II

||| parse one game line and return Handful cubes that can be the fewest number of cubes of each color
parseOneGame2 : String -> Maybe HandfulCubes
parseOneGame2 line =
    do
        handfuls <- (getHandfulsLine line)
        foldlM
            (\result, str => do
                h <- parseHandful str
                pure $ 
                    { red := if (h.red > result.red) then h.red else result.red
                    , green := if (h.green > result.green) then h.green else result.green
                    , blue := if (h.blue > result.blue) then h.blue else result.blue
                    } result
            )
            (MkHandfulCubes 0 0 0)
            (forget $ split (== ';') $ handfuls)

calculatePower : Maybe HandfulCubes -> Integer
calculatePower Nothing = 0
calculatePower (Just x) = x.red * x.green * x.blue

partTwo: List String -> Integer
partTwo [] = 0
partTwo (l::ls) =
    (calculatePower . parseOneGame2) l + partTwo ls


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    printLn $ "Part One result: " ++ (show $ partOne $ lines content)
    printLn $ "Part Two result: " ++ (show $ partTwo $ lines content)


main : IO ()
main = do
    run "aoc2023/day02/test.txt"
    run "aoc2023/day02/input.txt"
