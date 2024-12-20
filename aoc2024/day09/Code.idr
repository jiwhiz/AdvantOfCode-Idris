module Main

import Data.List
import Data.SortedMap
import Util
import Debug.Trace

charToDigit : Char -> Nat
charToDigit c =
  if c >= '0' && c <= '9' then
    -- ord '0' is the code for '0'
    cast (ord c - ord '0')
  else
    0


diskMap : String -> (Int, SortedMap Int Int)
diskMap mapStr = build (unpack mapStr) 0 0 empty
    where
        fillFile : Nat -> Int -> Int -> SortedMap Int Int -> SortedMap Int Int
        fillFile 0 _ _ map = map
        fillFile (S k) index id map = fillFile k (index + 1) id (insert index id map)

        build : List Char -> Int -> Int -> SortedMap Int Int -> (Int, SortedMap Int Int)
        build [] index id map = (index, map)
        build [fl] index id map =
            let fileLen = charToDigit fl
            in (index + (cast fileLen), fillFile fileLen index id map)
        build (fl :: sl :: rest) index id map =
            let fileLen = charToDigit fl
                spaceLen = charToDigit sl
            in build rest (index + (cast fileLen) + (cast spaceLen)) (id + 1) (fillFile fileLen index id map)


compact : (Int, SortedMap Int Int) -> (Int, SortedMap Int Int)
compact (lastIndex, map) = (lastIndex, go 0 lastIndex map) 
    where
        go : Int -> Int -> SortedMap Int Int -> SortedMap Int Int
        go left right map =
            if (left > right ) then map
            else case (lookup left map, lookup right map) of
                (Nothing, Nothing) => go left (right - 1) map
                (Nothing, (Just id)) => go (left + 1) (right - 1) (insert left id (delete right map))
                ((Just id), _) => go (left + 1) right map


checksum : (Int, SortedMap Int Int) -> Integer
checksum (lastIndex, map) = check 0 map 0
    where
        check : Int -> SortedMap Int Int -> Integer -> Integer
        check index map sum =
            if (index > lastIndex) then sum
            else case lookup index map of
                      Nothing => check (index + 1) map sum
                      (Just id) => check (index + 1) map (sum + cast (index * id))


compact2 : (Int, SortedMap Int Int) -> (Int, SortedMap Int Int)
compact2 (lastIndex, dmap) = (lastIndex, go lastIndex dmap)
    where
        removeBlock : Int -> Int -> Int -> SortedMap Int Int -> (Int, SortedMap Int Int)
        removeBlock right id length map =
            case lookup right map of
                Nothing => (length, map)
                Just id' =>
                    if id == id' then
                        removeBlock (right - 1) id (length + 1) (delete right map) 
                    else
                        (length, map)

        
        findSpace : Int -> Int -> Int -> SortedMap Int Int -> Int
        findSpace id length left map = scan length left
            where
                scan : Int -> Int -> Int
                scan len index =
                    if len == 0 then left
                    else case lookup index map of
                        Nothing => scan (len - 1) (index + 1) 
                        Just id' => 
                            if (id == id') then index
                            else findSpace id length (index + 1) map

        insertBlock : Int -> Int -> Int -> SortedMap Int Int -> SortedMap Int Int
        insertBlock id length index map =
            if (length == 0) then map 
            else insertBlock id (length - 1) (index + 1) (insert index id map)


        go : Int -> SortedMap Int Int -> SortedMap Int Int
        go right map =
            if (right < 0) then map 
            else case lookup right map of
                Nothing => go (right - 1) map
                Just id => 
                    let (length, map') = removeBlock right id 0 map
                        index = findSpace id length 0 map -- use the original map with id block
                    in
                    go (right - length) (insertBlock id length index map')


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        printLn $ "Part One result: " ++ show (checksum $ compact $ diskMap content)
        printLn $ "Part Two result: " ++ show (checksum $ compact2 $ diskMap content)


main : IO ()
main = do
    run "aoc2024/day09/test.txt"
    run "aoc2024/day09/input.txt"
