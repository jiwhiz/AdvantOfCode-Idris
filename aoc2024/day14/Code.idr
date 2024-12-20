module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap
import Data.String
import Data.String.Parser
import Util


repeatN : Nat -> (a -> a) -> a -> a
repeatN Z f x = x
repeatN (S n) f x = repeatN n f (f x)


Point : Type
Point = (Integer, Integer)


record Robot where
    constructor MkRobot
    p : Point
    v : Point

Show Robot where
    show r = "p:" ++ show r.p ++ " v:" ++ show r.v

parseP : Monad m => ParseT m Point
parseP = do
    skip $ string "p="
    x <- integer
    skip $ string ","
    y <- integer
    skip $ char ' '
    pure (x, y)


parseV : Monad m => ParseT m Point
parseV = do
    skip $ string "v="
    x <- integer
    skip $ string ","
    y <- integer
    pure (x, y)


parseRobot : Monad m => ParseT m Robot
parseRobot = MkRobot <$> parseP <*> parseV <* many (char '\n')


navigate : Integer -> Integer -> List Robot -> List Robot
navigate width height robots = go robots
    where
        move : Robot -> Robot
        move (MkRobot (px, py) v@(vx, vy)) = MkRobot (mod (px+vx) width, mod (py+vy) height) v
        

        go : List Robot -> List Robot
        go [] = []
        go (r::rs) = move r :: go rs


safetyFactor : Integer -> Integer -> List Robot -> Int
safetyFactor mwidth mheight robots =
    (sum $ (\(MkRobot (px, py) _) => if px < mwidth && py < mheight then 1 else 0) <$> robots) *
    (sum $ (\(MkRobot (px, py) _) => if px > mwidth && py < mheight then 1 else 0) <$> robots) *
    (sum $ (\(MkRobot (px, py) _) => if px < mwidth && py > mheight then 1 else 0) <$> robots) *
    (sum $ (\(MkRobot (px, py) _) => if px > mwidth && py > mheight then 1 else 0) <$> robots)


pictureString : Integer -> Integer -> List Robot -> String
pictureString width height robots =
    let
        grid = buildGrid robots SortedMap.empty
    in
        printGrid height grid
    where
        buildGrid : List Robot -> SortedMap Point () -> SortedMap Point ()
        buildGrid [] map = map
        buildGrid ((MkRobot p _) :: rest) map = buildGrid rest (SortedMap.insert p () map)

        rangeInteger : Integer -> List Integer
        rangeInteger n = go 0
            where
                go : Integer -> List Integer
                go i = if i < n then i :: go (i + 1) else []


        printRow : Integer -> SortedMap Point () -> String
        printRow h grid =
            pack $ 
                (\w => case lookup (w,h) grid of
                    Nothing => ' '
                    Just _ => '#'
                )
                <$> rangeInteger width

        printGrid : Integer -> SortedMap Point () -> String
        printGrid 0 grid = ""
        printGrid h grid = 
            printRow (height - h) grid ++
            printGrid (h - 1) grid

-- | A record representing a character and its count
record RLEEntry where
    constructor MkRLEEntry
    count : Nat
    char : Char

-- | Convert an RLEEntry to String, e.g., (3, '#') -> "3#"
rleEntryToString : RLEEntry -> String
rleEntryToString (MkRLEEntry count char) = show count ++ show char

-- | Run-Length Encoding: Compress a string using RLE
compress : String -> String
compress "" = ""
compress s = concatMap rleEntryToString (groupRLE s)
  where
    -- | Convert a list of identical characters to RLEEntry
    toRLEEntry : List1 Char -> RLEEntry
    toRLEEntry chars = MkRLEEntry (length chars) (head chars)

    -- | Groups the string into RLE entries
    groupRLE : String -> List RLEEntry
    groupRLE str = map toRLEEntry (group $ unpack str)


findTree : List Robot -> (Int, Nat)
findTree robots = run 0 (0, 10000000) robots
    where 
        run : Int -> (Int, Nat) -> List Robot -> (Int, Nat)
        run count (num, minLen) r =
            let next = navigate 101 103 r
                comp = compress $ pictureString 101 103 r
                e = length comp
                v = if (e < minLen) then (count, e) else (num, minLen)
            in 
            if (count > 10000) then v else run (count + 1) v next


run : String -> Integer -> Integer -> IO ()
run filename width height =
    do
        printLn filename
        content <- Util.loadFile filename
        let (Right (robots, _)) := parse (some parseRobot) content | Left errMsg => printLn ("Parse error: " ++ errMsg)
        let robots' = repeatN 100 (navigate width height) robots
        printLn $ "Part One result: " ++ (show $ safetyFactor (width `div` 2) (height `div` 2) robots')
        printLn $ "Part Two result: " ++ (show $ fst $ findTree robots)


main : IO ()
main = do
    run "aoc2024/day14/test.txt" 11 7
    run "aoc2024/day14/input.txt" 101 103
