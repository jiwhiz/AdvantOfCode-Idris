module Main

import Data.String
import Data.Vect
import Util


||| calculate the difference values of the sequence
diff : Vect (S k) Integer -> Vect k Integer
diff [x] = []
diff (x1 :: x2 :: xs) = (x2 - x1) :: diff (x2 :: xs)

||| extrapolated value of each line is the last numebr plus the extrapolated value of diff sequence
extrapolate : Vect n Integer -> Integer
extrapolate [] = 0
extrapolate input@(x::xs) = (head . reverse) input + extrapolate (diff input)


run : String -> IO ()
run filename = do
    printLn filename
    content <- loadFile filename

    putStrLn $ "Part One result: " ++ 
        (show $ sum <$> (traverse ((\l => extrapolate <$> (toVect (length l) l)) . parseIntegers) $ lines content ))
    putStrLn $ "Part Two result: " ++ 
        (show $ sum <$> (traverse ((\l => extrapolate <$> (toVect (length l) l)) . reverse . parseIntegers) $ reverse $ lines content))


main : IO ()
main = do
    run "aoc2023/day09/test.txt"
    run "aoc2023/day09/input.txt"
