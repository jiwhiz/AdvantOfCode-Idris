module Util

import Data.String
import Data.SortedMap
import System.File

export covering
loadFile : (path : String) -> IO (String)
loadFile path =
    let
        covering go : String -> File -> IO (Either FileError String)
        go acc file = do
            False <- fEOF file | True => pure (Right acc)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            go (acc ++ line) file
    in do
        result <- withFile path Read pure (go "")
        case result of
            (Left err) => pure ""
            (Right str) => pure str 


||| parse list of integers separated by space
export
parseIntegers : String -> List Integer
parseIntegers str = reverse $ 
    foldl
        (\acc, elm =>
            case parseInteger elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (words str)


export
displayGrid : Int -> Int -> SortedMap (Int, Int) Char -> IO ()
displayGrid width height grid =
    printGrid height grid
    where
        rangeInteger : Int -> List Int
        rangeInteger n = go 0
            where
                go : Int -> List Int
                go i = if i < n then i :: go (i + 1) else []


        printRow : Int -> SortedMap (Int, Int) Char -> IO ()
        printRow row grid = do 
            printLn $ pack $ 
                (\col => case lookup (row, col) grid of
                    Nothing => ' '
                    Just c => c
                )
                <$> rangeInteger width

        printGrid : Int -> SortedMap (Int, Int) Char -> IO ()
        printGrid 0 grid = do pure ()
        printGrid h grid = do
            printRow (height - h) grid
            printGrid (h - 1) grid
