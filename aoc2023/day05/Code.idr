module Main

import Data.Fin
import Data.String
import Data.List
import Data.List1
import Util


record AMap where
    constructor MkAMap
    desStart : Integer
    srcStart : Integer
    range : Integer

Show AMap where
    show a = "AMap: [desStart=" ++ show a.desStart ++ ", srcStart=" ++ show a.srcStart ++ ", range=" ++ show a.range ++ "]"


parseAMap : String -> Maybe AMap
parseAMap str =
    case words str of
        s1 :: s2 :: s3 :: [] => do
            desStart <- parseInteger s1
            srcStart <- parseInteger s2
            range <- parseInteger s3 
            pure $ MkAMap desStart srcStart range
        _ => Nothing


parseSeeds : String -> List Integer
parseSeeds str =
    reverse $ foldl
        (\acc, elm =>
            case parseInteger elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (words str)


mapChain : List (List AMap) -> Integer -> Integer
mapChain [] src = src
mapChain (x :: xs) src = mapChain xs (mapNumber src x)
    where
        mapNum : Integer -> AMap -> Maybe Integer
        mapNum src m =
            if src >= m.srcStart && src < m.srcStart + m.range then Just (src + m.desStart - m.srcStart) else Nothing

        mapNumber : Integer -> List AMap -> Integer
        mapNumber src [] = src
        mapNumber src (x :: xs) =
            case mapNum src x of
                Nothing => mapNumber src xs
                (Just y) => y


part1  : List Integer -> List (List AMap) -> Integer
part1 seeds chains = foldl min 99999999999 $ ((mapChain chains) <$> seeds)


pairSeeds : List Integer -> List (Integer, Integer)
pairSeeds (x :: y :: xs) = (x, y) :: pairSeeds xs
pairSeeds _ = []


scan : (Integer, Integer) -> List AMap -> List (Integer, Integer)
scan block [] = [block]
scan (start, width) ((MkAMap desStart srcStart range) :: xs) =
    if (start < srcStart && start + width > srcStart + range) then -- totally cover, split to three pieces
        (desStart, range) :: (scan (start, srcStart - start) xs ++ scan (srcStart + range, start + width - srcStart - range) xs)
    else if start < srcStart && start + width > srcStart && start + width <= srcStart + range then -- left overlap
        (desStart, start + width - srcStart) :: scan (start, srcStart - start) xs  -- split, return overlapping part, scan left cut off 
    else if start >= srcStart && start < srcStart + range && start + width > srcStart + range then -- right overlap
        (desStart + start - srcStart, srcStart + range - start) :: scan (srcStart + range, start +width - srcStart - range) xs -- split, return overlapping part, scan right cut off
    else if (start >= srcStart && start + width <= srcStart + range) then --  within map
        [(start + desStart - srcStart, width)] -- map to destination directly
    else scan (start, width) xs -- skip this map, continue remaining maps


mapRanges : List (Integer, Integer) -> List AMap -> List (Integer, Integer)
mapRanges ranges chain =
    concatMap (\r => scan r chain) ranges


part2 : List Integer -> List (List AMap) -> Integer
part2 seeds chains =
    let
        positions = foldl mapRanges (pairSeeds seeds) chains
    in foldl min 999999999 $ (fst <$> positions)


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename
    let (seedsLine::emptyLine::mapBlocks) := lines content | _ => printLn "No data"
    let seeds := parseSeeds seedsLine
    let chains := (mapMaybe parseAMap) <$> (forget $ split (=="") mapBlocks)
    putStrLn $ "Part One result: " ++ (show $ part1 seeds chains)
    putStrLn $ "Part Two result: " ++ (show $ part2 seeds chains)


main : IO ()
main = do
    run "aoc2023/day05/test.txt"
    run "aoc2023/day05/input.txt"
