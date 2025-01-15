module Main

import Data.Bits
import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap
import Data.String
import Data.String.Parser
import Util
import Data.Queue


data Part = Wire String | Signal Bits16

Show Part where
    show = \case { Wire s => s; Signal v => show v;}

data Inst
    = Pass Part String
    | And Part Part String
    | Or Part Part String
    | Not Part String
    | LShift Part (Fin 16) String
    | RShift Part (Fin 16) String

Show Inst where
    show = \case
            Pass input out => "( " ++ show input ++ " -> " ++ out ++ " )"
            And in1 in2 out => "( " ++ show in1 ++ " AND " ++ show in2 ++ " -> " ++ out ++ " )"
            Or in1 in2 out => "( " ++ show in1 ++ " OR " ++ show in2 ++ " -> " ++ out ++ " )"
            Not input out => "( NOT " ++ show input ++ " -> " ++ out ++ " )"
            LShift input n out => "( " ++ show input ++ " LSHIFT " ++ show n ++ " -> " ++ out ++ " )"
            RShift input n out => "( " ++ show input ++ " RSHIFT " ++ show n ++ " -> " ++ out ++ " )"


parseInput : String -> Part
parseInput str =
    case parsePositive str of
        Nothing => Wire str
        Just i => Signal i


passParser : Parser Inst
passParser = do
    input <- takeWhile isAlphaNum
    token " -> "
    output <- takeWhile isAlphaNum
    pure (Pass (parseInput input) output)


andParser : Parser Inst
andParser = do
    input1 <- takeWhile isAlphaNum
    token " AND "
    input2 <- takeWhile isAlphaNum
    token " -> "
    output <- takeWhile isAlphaNum
    pure (And (parseInput input1) (parseInput input2) output)


orParser : Parser Inst
orParser = do
    input1 <- takeWhile isAlphaNum
    token " OR "
    input2 <- takeWhile isAlphaNum
    token " -> "
    output <- takeWhile isAlphaNum
    pure (Or (parseInput input1) (parseInput input2) output)


notParser : Parser Inst
notParser = do
    token "NOT"
    input <- takeWhile isAlphaNum
    token " -> "
    output <- takeWhile isAlphaNum
    pure (Not (parseInput input) output)


lShiftParser : Parser Inst
lShiftParser = do
    input <- takeWhile isAlphaNum
    token " LSHIFT "
    number <- natural
    token " -> "
    output <- takeWhile isAlphaNum
    case isLT number 16 of
        Yes prf => pure (LShift (parseInput input) (natToFinLT number) output)
        No _ => fail "LSHIFT more than 15 bits"


rShiftParser : Parser Inst
rShiftParser = do
    input <- takeWhile isAlphaNum
    token " RSHIFT "
    number <- natural
    token " -> "
    output <- takeWhile isAlphaNum
    case isLT number 16 of
        Yes prf => pure (RShift (parseInput input) (natToFinLT number) output)
        No _ => fail "RSHIFT more than 15 bits"


instParser : Parser Inst
instParser = passParser <|> andParser <|> orParser <|> notParser <|> lShiftParser <|> rShiftParser


runCircuit : Queue Inst -> SortedMap String Bits16 -> SortedMap String Bits16
runCircuit queue map =
    case dequeue queue of
        Nothing => map
        Just (inst, queue') =>
            case process inst map of
                Nothing => runCircuit (enqueue inst queue') map
                Just map' => runCircuit queue' map'
    where
        findInput : Part -> SortedMap String Bits16 -> Maybe Bits16
        findInput (Signal v) _ = Just v
        findInput (Wire s) map = do
            v <- lookup s map
            pure v

        process : Inst -> SortedMap String Bits16 -> Maybe (SortedMap String Bits16)
        process (Pass input output) map = do
                i <- findInput input map
                pure (insert output i map)
        process (And input1 input2 output) map = do
                i1 <- findInput input1 map
                i2 <- findInput input2 map
                pure (insert output (i1 .&. i2) map)
        process (Or input1 input2 output) map = do
                i1 <- findInput input1 map
                i2 <- findInput input2 map
                pure (insert output (i1 .|. i2) map)
        process (Not input output) map = do
                i <- findInput input map
                pure (insert output (complement i) map)
        process (LShift input number output) map = do
                i <- findInput input map
                pure (insert output (shiftL i number) map)
        process (RShift input number output) map = do
                i <- findInput input map
                pure (insert output (shiftR i number) map)


part1 : List Inst -> SortedMap String Bits16
part1 insts =
    let queue = foldl (\q, inst => enqueue inst q) Queue.empty insts
    in runCircuit queue SortedMap.empty


part2 : List Inst -> SortedMap String Bits16
part2 insts =
    let insts' = filter (\case {Pass _ "b" => False; _ => True} ) insts
        queue = foldl (\q, inst => enqueue inst q) Queue.empty insts'
    in runCircuit queue (singleton "b" 3176)


run : String -> IO ()
run filename = do
        printLn filename
        content <- Util.loadFile filename
        let (Right result) := traverse (parse instParser) (lines content) | Left errMsg => printLn ("Parse error: " ++ errMsg)
        let insts := map fst result
        putStrLn $ "Part One result: " ++ (show $ part1 insts)
        putStrLn $ "Part Two result: " ++ (show $ part2 insts)


main : IO ()
main = do
    run "aoc2015/day07/test.txt"
    run "aoc2015/day07/input.txt"
