module Main

import Data.List1
import Data.String
import Data.String.Parser
import Util

anyChar : Applicative m => ParseT m Char
anyChar = P $ \s =>
    if s.pos < s.maxPos then
        let ch = assert_total $ strIndex s.input s.pos in
        pure $ OK ch (S s.input (s.pos + 1) s.maxPos)
    else
        pure $ Fail s.pos "unexpected end of input"


data Inst : Type where
    Mult : Integer -> Integer -> Inst
    Do : Inst
    Dont : Inst


-- Parse exactly the format: mul(<int>,<int>)
mulParser : Monad m => ParseT m Inst
mulParser = do
    skip $ string "mul("
    x <- integer   -- integer returns Integer
    skip $ char ','
    y <- integer   -- integer returns Integer
    skip $ char ')'
    pure $ Mult x y


doParser : Monad m => ParseT m Inst
doParser = do 
    skip $ string "do()"
    pure Do


dontParser : Monad m => ParseT m Inst
dontParser = do 
    skip $ string "don't()"
    pure Dont


instParser : Monad m => ParseT m Inst
instParser = mulParser <|> doParser <|> dontParser


mutual
    some' : Monad m => ParseT m a -> ParseT m (List a)
    some' p = do
        r <- p
        rs <- many' p
        pure $ r :: rs

    many' : Monad m => ParseT m a -> ParseT m (List a)
    many' p = some' p <|> (anyChar *> many' p) <|> pure []

allParser : Monad m => ParseT m (List Inst)
allParser =
    many' instParser


value : Inst -> Integer
value (Mult x y) = x * y
value _ = 0

part2 : List Inst -> Bool -> Integer -> Integer
part2 [] _ acc = acc
part2 (Do :: insts) _ acc = part2 insts True acc 
part2 (Dont :: insts) _ acc = part2 insts False acc 
part2 (_ :: insts) False acc = part2 insts False acc
part2 (Mult x y :: insts) True acc = part2 insts True (acc + x * y) 


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let (Right (insts, pos)) := parse allParser content | Left errMsg => printLn ("Parse error: " ++ errMsg)
        printLn $ "Part One result: " ++ show (foldl (+) 0 (map value insts))
        printLn $ "Part Two result: " ++ show (part2 insts True 0)


main : IO ()
main = do
    run "aoc2024/day03/test1.txt"
    run "aoc2024/day03/test2.txt"
    run "aoc2024/day03/input.txt"
