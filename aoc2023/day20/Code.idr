module Main

import Data.Fuel
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.String.Extra
import Data.String.Parser
import Control.Monad.State
import Util
import Data.Queue

data ModuleState
    = FlipFlop Bool
    | Conjunction (SortedMap String Bool)
    | Broadcaster

Show ModuleState where
    show (FlipFlop b) = "FlipFlop (" ++ (show b) ++ ")"
    show (Conjunction mem) = "Conjunction (" ++ (show $ SortedMap.toList mem) ++ ")"
    show Broadcaster = "Broadcaster"

record Module where
    constructor MkModule
    name : String
    state : ModuleState
    output : List String

Show Module where
    show m = "Module:" ++ m.name ++ "[" ++ (show m.state) ++ "] -> (" ++ (joinBy "," m.output) ++ ")"


parseModule : String -> Maybe Module
parseModule str =
    if isPrefixOf "broadcaster" str then
        Just $ MkModule "broadcaster" Broadcaster $ map trim $ forget $ split (==',') $ drop 14 str
    else if isPrefixOf "%" str then
        Just $ MkModule (fst $ break isSpace $ drop 1 str) (FlipFlop False) $ map trim $ forget $ split (==',') $ drop 1 $ snd $ break (=='>') str
    else if isPrefixOf "&" str then
        Just $ MkModule (fst $ break isSpace $ drop 1 str) (Conjunction SortedMap.empty) $ map trim $ forget $ split (==',') $ drop 1 $ snd $ break (=='>') str
    else Nothing


pushButton : SortedMap String Module -> (SortedMap String Module, List Bool)
pushButton mMap =
    let queue = Queue.singleton ("button", False, "broadcaster")
    in runState mMap (process queue)
    where
        process : Queue (String, Bool, String) -> State (SortedMap String Module) (List Bool)
        process queue =
            do
                moduleMap <- get
                let Just ((input, freq, output), q) := dequeue queue | Nothing => pure []  -- finished
                case lookup output moduleMap of
                    Nothing => do 
                        next <- process q
                        pure (freq :: next)
                    Just m =>
                        case m.state of
                            Broadcaster => do
                                next <- process (foldl (\q', o => enqueue (output, False, o) q') q m.output)
                                pure (freq :: next)
                            FlipFlop s =>
                                if freq then do
                                    next <- process q
                                    pure (freq :: next)
                                else do
                                    modify (insert output ({state := FlipFlop (not s)} m)) -- switch on/off
                                    next <- process (foldl (\q', o => enqueue (output, not s, o) q') q m.output)
                                    pure (freq :: next)
                            Conjunction inputMap => do
                                let map' := insert input freq inputMap
                                let newFreq := (length $ filter not $ values map') > 0 -- at least one low input, send out high
                                modify (insert output ({state := Conjunction map'} m))
                                next <- process (foldl (\q', o => enqueue (output, newFreq, o) q') q m.output)
                                pure (freq :: next)


initConjunctionStates : List Module -> SortedMap String Module
initConjunctionStates moduleList =
    let moduleMap = SortedMap.fromList $ map (\m => (m.name, m)) $ moduleList
    in SortedMap.fromList $ map (\m => (m.name, m)) $ go moduleMap moduleList
    where
        go : SortedMap String Module -> List Module -> List Module
        go _ [] = []
        go moduleMap (m::rest) =
            case m.state of
                Conjunction inputMap => 
                    ( { state := Conjunction $
                        foldl
                            (\imap', otherModule =>
                                foldl (\imap'', out => if out == m.name then insert otherModule.name False imap'' else imap'')
                                imap' otherModule.output
                            )
                            inputMap (values moduleMap)
                    } m
                    )
                    :: go moduleMap rest 
                _ => m :: go moduleMap rest


part1 : SortedMap String Module -> Integer
part1 mMap =
    let (high, low) = loop 1000 mMap
    in cast (high * low)
    where
        loop : Nat -> SortedMap String Module -> (Nat, Nat)
        loop Z _ = (0, 0)
        loop (S k) mMap =
            let (mMap', pulses) = pushButton mMap 
                (h, l) = loop k mMap'
            in (h + (length $ filter id pulses), l + (length $ filter not pulses))


part2 : SortedMap String Module -> Integer
part2 mMap =
    case (head' $ map (\m => m.name) $ filter (\m => (List.length $ filter (== "rx") m.output) > 0) $ values mMap) of
        Nothing => 0
        Just rxInput => -- found module names which pass pulse to rx module
            let m = pushButton (limit 100000000) rxInput mMap empty 1 in
            Util.lcm $ (cast <$> values m)
    where
        process : String -> Nat -> Queue (String, Bool, String) -> State (SortedMap String Module, SortedMap String Nat) Bool
        process rxInput count queue = do
                (moduleMap, countMap) <- get
                let Just ((input, freq, output), q) := dequeue queue | Nothing => pure False  -- finished queue
                let countMap' := if (output == rxInput && freq) then insert input count countMap else countMap
                put (moduleMap, countMap')
                case lookup output moduleMap of
                        Nothing =>
                            process rxInput count q
                        Just m =>
                            case m.state of
                                Broadcaster =>
                                    process rxInput count (foldl (\q', o => enqueue (output, False, o) q') q m.output)
                                FlipFlop s =>
                                    if freq then process rxInput count q
                                    else do
                                        put (insert output ({state := FlipFlop (not s)} m) moduleMap, countMap') -- switch on/off
                                        process rxInput count (foldl (\q', o => enqueue (output, not s, o) q') q m.output)
                                Conjunction inputMap => do
                                    let map' := insert input freq inputMap
                                    let newFreq := (length $ filter not $ values map') > 0 -- at least one low input, send out high
                                    put (insert output ({state := Conjunction map'} m) moduleMap, countMap')
                                    process rxInput count (foldl (\q', o => enqueue (output, newFreq, o) q') q m.output)

        pushButton : Fuel -> String -> SortedMap String Module -> SortedMap String Nat -> Nat -> SortedMap String Nat
        pushButton Dry _ _ m _ = m
        pushButton (More fuel) rxInput moduleMap countMap count =
            let ((moduleMap', countMap'), _) = runState (moduleMap, countMap) (process rxInput count $ Queue.singleton ("button", False, "broadcaster"))
            in if (length $ SortedMap.keys countMap') >= 4 then countMap' else pushButton fuel rxInput moduleMap' countMap' (S count)


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename

        let modules := mapMaybe id $ parseModule <$> lines content
        let moduleMap := initConjunctionStates modules
        printLn $ "Part One result: " ++ show (part1 moduleMap)
        printLn $ "Part Two result: " ++ show (part2 moduleMap)


main : IO ()
main = do
    --run "aoc2023/day20/test1.txt"
    --run "aoc2023/day20/test2.txt"
    run "aoc2023/day20/input.txt"
