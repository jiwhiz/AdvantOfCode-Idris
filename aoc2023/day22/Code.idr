module Main

import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Data.SortedMap
import Util
import Debug.Trace

record Brick where
    constructor MkBrick
    x1 : Int
    y1 : Int
    z1 : Int
    x2 : Int
    y2 : Int
    z2 : Int

Eq Brick where
    (==) (MkBrick x1 y1 z1 x2 y2 z2) (MkBrick x1' y1' z1' x2' y2' z2') = 
        x1==x1' && y1 == y1' && z1 == z1' && x2 == x2' && y2 == y2' && z2 == z2'

Show Brick where
    show b = "(" ++ show b.x1 ++ "," ++ show b.y1 ++ "," ++ show b.z1 ++ ") ~ (" ++ show b.x2 ++ "," ++ show b.y2 ++ "," ++ show b.z2 ++ ")"


parseData : List String -> List Brick
parseData = mapMaybe parseBrick 
    where
        parseBrick : String -> Maybe Brick
        parseBrick str = do
            let (coord1, coord2) := break (=='~') str
            let (x1::y1::z1::_) := forget $ split (==',') coord1 | _ => Nothing
            let (x2::y2::z2::_) := forget $ split (==',') $ drop 1 coord2 | _ => Nothing
            x1' <- parsePositive x1
            y1' <- parsePositive y1
            z1' <- parsePositive z1
            x2' <- parsePositive x2
            y2' <- parsePositive y2
            z2' <- parsePositive z2
            pure $ MkBrick x1' y1' z1' x2' y2' z2'


stabilize : List Brick -> List Brick
stabilize bList = go (sortBy (\(MkBrick _ _ z1 _ _ _), (MkBrick _ _ z1' _ _ _) => compare z1 z1') bList) []
    where
        go : List Brick -> List Brick -> List Brick
        go [] settled = settled
        go (b::rest) settled = go rest (settle b (sortBy (\(MkBrick _ _ _ _ _ z2), (MkBrick _ _ _ _ _ z2') => compare z2' z2) settled))
            where
                settle : Brick -> List Brick -> List Brick
                settle (MkBrick x1 y1 z1 x2 y2 z2) [] = [MkBrick x1 y1 1 x2 y2 (z2 - z1 + 1)]
                settle brick@(MkBrick x1 y1 z1 x2 y2 z2) (x@(MkBrick x1' y1' z1' x2' y2' z2') :: rest) =
                    if (x2 < x1' || x1 > x2' || y2 < y1' || y1 > y2') then -- not supported by
                        x :: settle brick rest
                    else -- supported, set brick to one level above x in z coordinate
                        (MkBrick x1 y1 (z2' + 1) x2 y2 (z2 - z1 + z2' + 1)) :: x :: rest


part1 : List Brick -> Int
part1 list = go list
    where
        isSupportedBy : Brick -> Brick -> Bool
        isSupportedBy (MkBrick x1 y1 z1 x2 y2 z2) (MkBrick x1' y1' z1' x2' y2' z2') =
            x2 >= x1' && x1 <= x2' && y2 >= y1' && y1 <= y2' && z1 == z2' + 1

        hasSupport : Brick -> List Brick -> Bool
        hasSupport b bl =
            foldl (\result, x=> result || isSupportedBy b x) False bl

        check : Brick -> Int
        check b@(MkBrick x1 y1 z1 x2 y2 z2) =
            let supported = filter (\x => isSupportedBy x b) list
                supporting = filter (\x@(MkBrick x1' y1' z1' x2' y2' z2') => z2 == z2' && x /= b) list
                stillSupported = filter (\x => if hasSupport x supporting then True else False) supported
            in if length supported == length stillSupported then 1 else 0

        go : List Brick -> Int
        go [] = 0
        go (b::rest) = check b + go rest


part2 : List Brick -> Int
part2 list = go [] $ reverse list
    where
        check : List Brick -> List Brick -> Int
        check [] settled = 0
        check (b::rest) settled =
            let (count, settled') = settle b (sortBy (\(MkBrick _ _ _ _ _ z2), (MkBrick _ _ _ _ _ z2') => compare z2' z2) settled)
            in count + check rest settled'
            where
                settle : Brick -> List Brick -> (Int, List Brick)
                settle brick@(MkBrick x1 y1 z1 x2 y2 z2) [] =
                    if z1 == 1 then (0, [brick])
                    else (1, [MkBrick x1 y1 1 x2 y2 (z2 - z1 + 1)])
                settle brick@(MkBrick x1 y1 z1 x2 y2 z2) (x@(MkBrick x1' y1' z1' x2' y2' z2') :: rest) =
                    if (x2 < x1' || x1 > x2' || y2 < y1' || y1 > y2') then
                        let (c, s) = settle brick rest
                        in (c, x::s)
                    else
                        if z1 == z2' + 1 then (0, brick::x::rest)
                        else (1, (MkBrick x1 y1 (z2' + 1) x2 y2 (z2 - z1 + z2' + 1)) :: x :: rest)

        go : List Brick -> List Brick -> Int
        go _ [] = 0
        go checked (b::rest) = check rest checked + go (checked ++ [b]) rest



run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let bricks := stabilize $ parseData $ lines content

        putStrLn $ "Part One result: " ++ (show $ part1 bricks)
        putStrLn $ "Part Two result: " ++ (show $ part2 bricks)
        pure ()


main : IO ()
main = do
    run "aoc2023/day22/test.txt"
    run "aoc2023/day22/input.txt"
