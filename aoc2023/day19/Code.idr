module Main

import Data.List
import Data.List1
import Data.String
import Data.SortedMap
import Util


data Category = X | M | A | S

data Target = Jump String | Accept | Reject

data Rule = Less Category Integer Target | More Category Integer Target | End Target

record Workflow where
    constructor MkWorkflow
    name : String
    rules : List Rule

Show Workflow where
    show wf = "Workflow " ++ wf.name

record Rating where
    constructor MkRating
    x : Integer
    m : Integer
    a : Integer
    s : Integer

Show Rating where
    show r = "{x=" ++ (show r.x) ++ ",m=" ++ (show r.m) ++ ",a=" ++ (show r.a) ++ ",s=" ++ (show r.s) ++ "}"

parseCategory : Char -> Maybe Category
parseCategory 'x' = Just X
parseCategory 'm' = Just M
parseCategory 'a' = Just A
parseCategory 's' = Just S
parseCategory _ = Nothing


parseTarget : String -> Target
parseTarget "A" = Accept
parseTarget "R" = Reject
parseTarget name = Jump name


parseRule : String -> Maybe Rule
parseRule str =
    let (c1::c2::cs) = unpack str | _ => Nothing
        (numStr, t) = break (== ':') (pack cs)
    in do
        category <- parseCategory c1
        num <- parsePositive numStr
        let target := parseTarget $ pack $ drop 1 $ unpack t
        rule <- case c2 of '<'=> Just (Less category num target); '>'=> Just (More category num target); _ => Nothing
        pure rule


parseWorkflow : String -> Workflow
parseWorkflow str =
    let name = fst $ break (== '{') str
        ruleStrList = split (== ',') $ pack $ drop 1 $ unpack $ snd $ break (== '{') $ fst $ break (== '}') str
        end = parseTarget $ head $ reverse ruleStrList 
        rules = mapMaybe id $ parseRule <$> (fst $ unsnoc ruleStrList)
    in MkWorkflow name (rules ++ [End end])


parseRating : String -> Rating
parseRating str =
    go (forget $ split (== ',') $ pack $ drop 1 $ unpack $ fst $ break (== '}') str) (MkRating 0 0 0 0)
    where
        go : List String -> Rating -> Rating
        go [] r = r
        go (x::xs) r =
            let Just num = parsePositive $ pack $ drop 1 $ unpack $ snd $ break (== '=') x | _ => r
            in case fst $ break (== '=') x of
                "x" => go xs $ { x := num } r
                "m" => go xs $ { m := num } r
                "a" => go xs $ { a := num } r
                "s" => go xs $ { s := num } r
                _ => r


part1 : SortedMap String Workflow -> List Rating -> Integer
part1 wfMap [] = 0
part1 wfMap (rating::rest) =
    process "in" rating + part1 wfMap rest
    where
        runRule : Rating -> List Rule -> Target 
        runRule _ [] = Reject -- error?
        runRule rating@(MkRating x m a s) (rule::rest) =
            case rule of
                Less X num target => if x < num then target else runRule rating rest
                Less M num target => if m < num then target else runRule rating rest
                Less A num target => if a < num then target else runRule rating rest
                Less S num target => if s < num then target else runRule rating rest
                More X num target => if x > num then target else runRule rating rest
                More M num target => if m > num then target else runRule rating rest
                More A num target => if a > num then target else runRule rating rest
                More S num target => if s > num then target else runRule rating rest
                End target => target

        process : String -> Rating -> Integer
        process wfName rating@(MkRating x m a s) =
            let Just wf = lookup wfName wfMap | _ => 0
            in case runRule rating wf.rules of
                Jump next => process next rating
                Accept => x + m + a + s 
                Reject => 0


record Range where
    constructor MkRange
    rx : (Integer, Integer)
    rm : (Integer, Integer)
    ra : (Integer, Integer)
    rs : (Integer, Integer)


part2 : SortedMap String Workflow -> Integer
part2 wfMap =
    let ranges = process "in" (MkRange (1,4000) (1,4000) (1,4000) (1,4000))
    in sum $ map (\(MkRange rx rm ra rs) => (snd rx - fst rx + 1) * (snd rm - fst rm + 1) * (snd ra - fst ra + 1) * (snd rs - fst rs + 1)) ranges
    where
        process : String -> Range -> List Range

        runRule : Range -> List Rule -> List Range 
        runRule _ [] = []
        runRule range@(MkRange rx rm ra rs) (rule::rest) =
            case rule of
                Less X num target =>
                    handle (MkRange (fst rx, num-1) rm ra rs) target ++
                    runRule (MkRange (num, snd rx) rm ra rs) rest
                Less M num target =>
                    handle (MkRange rx (fst rm, num-1) ra rs) target ++
                    runRule (MkRange rx (num, snd rm) ra rs) rest
                Less A num target =>
                    handle (MkRange rx rm (fst ra, num-1) rs) target ++
                    runRule (MkRange rx rm (num, snd ra) rs) rest
                Less S num target =>
                    handle (MkRange rx rm ra (fst rs, num-1)) target ++
                    runRule (MkRange rx rm ra (num, snd rs)) rest
                More X num target =>
                    handle (MkRange (num+1, snd rx) rm ra rs) target ++
                    runRule (MkRange (fst rx, num) rm ra rs) rest
                More M num target =>
                    handle (MkRange rx (num+1, snd rm) ra rs) target ++
                    runRule (MkRange rx (fst rm, num) ra rs) rest
                More A num target =>
                    handle (MkRange rx rm (num+1, snd ra) rs) target ++
                    runRule (MkRange rx rm (fst ra, num) rs) rest
                More S num target =>
                    handle (MkRange rx rm ra (num+1, snd rs)) target ++
                    runRule (MkRange rx rm ra (fst rs, num)) rest
                End target => handle range target
            where
                handle : Range -> Target -> List Range
                handle r t = case t of
                    Jump next => process next r
                    Accept => [r]
                    Reject => []

        process wfName range@(MkRange rx rm ra rs) =
            case lookup wfName wfMap of
                Nothing => []
                Just wf => runRule range wf.rules


run : String -> IO ()
run filename =
    do
        printLn filename
        content <- Util.loadFile filename
        let (p1, p2) := break (=="") $ lines content
        let workflows := SortedMap.fromList $ map (\wf => (wf.name, wf)) (parseWorkflow <$> p1)
        let ratings := parseRating <$> p2
        putStrLn $ "Part One result: " ++ (show $ part1 workflows ratings)
        putStrLn $ "Part Two result: " ++ (show $ part2 workflows)


main : IO ()
main = do
    run "aoc2023/day19/test.txt"
    run "aoc2023/day19/input.txt"
