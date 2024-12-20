module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.String
import Data.String.Parser
import Decidable.Equality
import Util
import Debug.Trace

anyChar : Applicative m => ParseT m Char
anyChar = P $ \s =>
    if s.pos < s.maxPos then
        let ch = assert_total $ strIndex s.input s.pos in
        pure $ OK ch (S s.input (s.pos + 1) s.maxPos)
    else
        pure $ Fail s.pos "unexpected end of input"


record Machine where
    constructor MkMachine
    a : Nat
    b : Nat
    c : Nat


Show Machine where
    show m = "A:" ++ show m.a ++ " B:" ++ show m.b ++ " C:" ++ show m.c

parseRegister : Monad m => ParseT m Nat
parseRegister = do
    skip $ token "Register"
    skip $ anyChar
    skip $ token ":"
    x <- integer
    pure (cast x)


parseMachine : Monad m => ParseT m Machine
parseMachine = MkMachine <$> parseRegister <*> parseRegister <*> parseRegister

parseIntList : String -> List Int
parseIntList str = List.reverse $ 
    foldl
        (\acc, elm =>
            case parseInteger elm of
                Just v => cast v :: acc
                Nothing => acc
        )
        []
        (map trim $ forget $ split (== ',') str)


getCombo : Int -> Machine -> Nat
getCombo 4 m = m.a
getCombo 5 m = m.b
getCombo 6 m = m.c
getCombo v _ = cast v

getCode : Nat -> List Int -> Maybe (Int, Int)
getCode _ [] = Nothing
getCode _ [x] = Nothing
getCode Z (x::y::insts) = Just (x, y)
getCode (S k) (x::insts) = getCode k insts

xor : Nat -> Nat -> Nat 
xor Z Z = Z 
xor a b =
    let a1 = modNatNZ a 2 SIsNonZero
        b1 = modNatNZ b 2 SIsNonZero
        ab = case (a1, b1) of
            (0, 0) => 0
            (0, 1) => 1
            (1, 0) => 1
            (1, 1) => 0
            _ => 0
    in ab + (xor (a `div` 2) (b `div` 2)) * 2


runProgram : List Int -> Machine -> (List Int, Machine)
runProgram instructions machine = run Z machine []
    where
        run : Nat -> Machine -> List Int -> (List Int, Machine)
        run ptr m output =
            let Just (opcode, operand) = getCode ptr instructions | Nothing => (output, m)
            in 
            case opcode of
                0 => let numerator = m.a
                         denominator = 2 `power` (getCombo operand m)
                         
                     in --case decEq denominator 0 of
                        case denominator of
                             Z => (output, m)  -- divided by zero, terminate
                             (S k) => 
                                let result = divNatNZ numerator denominator SIsNonZero in
                                run (S (S ptr)) (MkMachine result m.b m.c) output
                
                1 => run (S (S ptr)) (MkMachine m.a (xor m.b (cast operand)) m.c) output
                
                2 => run (S (S ptr)) (MkMachine m.a (modNatNZ (getCombo operand m) 8 SIsNonZero) m.c) output
                
                3 => case m.a of
                    Z => run (S (S ptr)) m output
                    S k => run (cast operand) m output

                4 => run (S (S ptr)) (MkMachine m.a (xor m.b m.c) m.c) output

                5 => run (S (S ptr)) m (cast (modNatNZ (getCombo operand m) 8 SIsNonZero) :: output)
                --5 => ((cast (modNatNZ (getCombo operand m) 8 SIsNonZero) :: output), m)

                6 => let numerator = m.a
                         denominator = 2 `power` (getCombo operand m)
                         
                     in --case decEq denominator 0 of
                        case denominator of
                             Z => (output, m)  -- divided by zero, terminate
                             (S k) => 
                                let result = divNatNZ numerator denominator SIsNonZero in
                                run (S (S ptr)) (MkMachine m.a result m.c) output

                7 => let numerator = m.a
                         denominator = 2 `power` (getCombo operand m)
                         
                     in --case decEq denominator 0 of
                        case denominator of
                             Z => (output, m)  -- divided by zero, terminate
                             (S k) => 
                                let result = divNatNZ numerator denominator SIsNonZero in
                                run (S (S ptr)) (MkMachine m.a m.b result) output

                _ => (output, m)


same : List Int -> List Int -> Bool
same [] [] = True
same [] (x::xs) = False
same (x::xs) [] = False
same (x::xs) (y::ys) = if x == y then same xs ys else False


btod : String -> Nat
btod str = cal (unpack str) 0
    where
        cal : List Char -> Nat -> Nat
        cal [] b = b
        cal (x::xs) b =
            case x of
                '1' => cal xs (b*2+1)
                '0' => cal xs (b*2)
                _ => b

getLastThreeBits : List Nat -> Nat
getLastThreeBits [] = Z
getLastThreeBits [x] = x
getLastThreeBits (x::[y]) = x * 2 + y
getLastThreeBits (x::y::[z]) = x * 4 + y * 2 + z
getLastThreeBits (x::y::z::rest) = getLastThreeBits (y::z::rest)

shift : List Nat -> Nat -> List Nat 
shift l n = reverse $ drop n (reverse l)


merge : List Nat -> Nat -> List Nat
merge n 0 = n ++ [0,0,0]
merge n 1 = n ++ [0,0,1]
merge n 2 = n ++ [0,1,0]
merge n 3 = n ++ [0,1,1]
merge n 4 = n ++ [1,0,0]
merge n 5 = n ++ [1,0,1]
merge n 6 = n ++ [1,1,0]
merge n 7 = n ++ [1,1,1]
merge n _ = n


part2 : List Nat -> List (List Nat)
part2 inst = go (singleton [] inst) []
    where
        go : SortedMap (List Nat) (List Nat) -> List (List Nat) -> List (List Nat)
        go map result =
            case SortedMap.toList map of
                [] => result -- finished search, return all results
                ((n, insts)::rest) =>
                    let map' = delete n map in
                    case insts of
                        [] => go map' (n::result) -- finished instrutions, return bit array as one result
                        (x::xs) =>
                            let b1 = xor x 4
                                bs = filter (\b => (xor b b1) == getLastThreeBits (shift (merge n (xor b 1)) b)) [0..7]
                            in go (foldl (\m, b => insert (merge n (xor b 1)) xs m) map' bs) result


bitListToInteger : List Nat -> Integer
bitListToInteger bitList = go $ reverse bitList
    where
        go : List Nat -> Integer
        go [] = 0
        go [x] = cast x
        go (x::xs) = (go xs) * 2 + (cast x)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (p1, p2) := break (=="") $ lines content
        let (Right (machine, _)) := parse parseMachine (joinBy "" p1) | Left errMsg => printLn ("Parse machine error: " ++ errMsg)
        let program := parseIntList $ snd $ break (== ' ') (joinBy "" p2)

        let (output, m) := runProgram program machine
        printLn $ "Part One result: " ++ (pack $ filter (not . (==) ' ') $unpack $ show $ reverse output)

        let (v::vs) := map bitListToInteger $ part2 $ (\i => cast i) <$> reverse program | _ => printLn "No result for part 2"
        printLn $ "Part Two result: " ++ (show $ foldl min v vs)


main : IO ()
main = do
    run "aoc2024/day17/test.txt"
    run "aoc2024/day17/input.txt"

