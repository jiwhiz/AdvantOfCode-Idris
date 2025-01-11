module Main

import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Data.SortedMap
import Data.Vect
import Util
import Debug.Trace

record Hailstone where
    constructor MkHailstone
    px : Integer
    py : Integer
    pz : Integer
    vx : Integer
    vy : Integer
    vz : Integer

Eq Hailstone where
    (==) (MkHailstone px py pz vx vy vz) (MkHailstone px' py' pz' vx' vy' vz') = 
        px == px' && py == py' && pz == pz' && vx == vx' && vy == vy' && vz == vz'

Ord Hailstone where
    compare (MkHailstone px py pz vx vy vz) (MkHailstone px' py' pz' vx' vy' vz') =
        let c1 : Vect 6 Integer = [px,py,pz,vx,vy,vz]
            c2 : Vect 6 Integer = [px',py',pz',vx',vy',vz']
        in compare c1 c2 

Show Hailstone where
    show h = "(" ++ show h.px ++ "," ++ show h.py ++ "," ++ show h.pz ++ ") @ (" ++ show h.vx ++ "," ++ show h.vy ++ "," ++ show h.vz ++ ")"


parseData : List String -> List Hailstone
parseData = mapMaybe parseBrick 
    where
        parseHailstone : String -> Maybe Hailstone
        parseBrick str = do
            let (coord1, coord2) := break (=='@') str
            let (px::py::pz::_) := forget $ split (==',') coord1 | _ => Nothing
            let (vx::vy::vz::_) := forget $ split (==',') $ drop 1 coord2 | _ => Nothing
            [| MkHailstone (parseInteger px) (parseInteger py) (parseInteger pz) 
                (parseInteger vx) (parseInteger vy) (parseInteger vz)|]


part1 : Integer -> Integer -> List Hailstone -> Int
part1 low high hailstones =
    foldl
        (\acc, (h1, h2) => if canCross h1 h2 then acc + 1 else acc
        )
        0
        [ (h1, h2) 
                | h1 <- hailstones
                , h2 <- hailstones
                , h1 < h2 ]
    where
        canCross : Hailstone -> Hailstone -> Bool
        canCross (MkHailstone px1 py1 _ vx1 vy1 _) (MkHailstone px2 py2 _ vx2 vy2 _) =
            if (vx1 * vy2 - vy1 * vx2) == 0 || (vx2 * vy1 - vy2 * vx1) == 0 then False
            else
                let t1 = (py1 * vx2 + px2 * vy2 - px1 * vy2 - py2 * vx2) `div` (vx1 * vy2 - vy1 * vx2)
                    t2 = (px1 * vy1 + py2 * vx1 - py1 * vx1 - px2 * vy1) `div` (vx2 * vy1 - vy2 * vx1)
                    x  = px1 + vx1 * t1
                    y  = py1 + vy1 * t1
                in (t1 > 0 && t2 > 0 && x >= low && x <= high && y >= low && y <= high)


-- Cramer's rule to solve linear equations

||| Produce a Vect n (Fin n) of valid column indices [0..n-1],
||| to be used in cofactor expansions or other operations
indicesOf : {n : Nat} -> Vect n a -> Vect n (Fin n)
indicesOf {n=Z}   []        = []
indicesOf {n=S k} (_ :: xs) = FZ :: map FS (indicesOf xs)

||| det: determinant by cofactor expansion along the first row
det : {n : Nat} -> Num a => Vect n (Vect n a) -> a
det {n=Z} _ = 1 -- By convention, determinant of a 0×0 matrix is 1
det {n=S Z} ((x :: Nil) :: Nil) = x
det {n=S n'} (row0 :: rows) =
    let idxs = indicesOf row0
    in sum (map (\i => cofactor i (index i row0)) idxs)
    where
        signFactor : Fin n -> a
        signFactor i =
        if (finToNat i) `mod` 2 == 0 then 1 else -1

        minor : (row : Fin (S n')) ->
                (col : Fin (S n')) ->
                Vect (S n') (Vect (S n') a) ->
                Vect n' (Vect n' a)
        minor row col mat = map (deleteAt col) (deleteAt row mat)

        cofactor : Fin (S n') -> a -> a
        cofactor i val =
            signFactor i * val * det (minor 0 i (row0 :: rows))

||| Solve M x = b using Cramer's Rule
solveCramer : {n : Nat} -> Vect n (Vect n Integer) -> Vect n Integer -> Vect n Integer
solveCramer m b =
    let d = det m
        idxs = indicesOf b
        solveOne : Fin n -> Integer
        solveOne i = (det (replaceColumn i b m)) `div` d
    in  map solveOne idxs
    where
        replaceColumn : {n : Nat} ->
                        (i : Fin n) ->
                        Vect n a ->
                        Vect n (Vect n a) ->
                        Vect n (Vect n a)
        replaceColumn i col mat =
            let rowValPairs = zip mat col
            in  map (\(row, val) => updateAt i (const val) row) rowValPairs


part2 : List Hailstone -> Integer
part2 hailstones =
    let (h0::h1::h2::h3::rest) = hailstones | _ => 0
        matrix = (addM h0 h1) ++ (addM h0 h2) ++ (addM h0 h3)
        b = (addB h0 h1) ++ (addB h0 h2) ++ (addB h0 h3)
        (x::y::z::_) = solveCramer matrix b
    in x + y + z 
    where
        addM : Hailstone -> Hailstone -> Vect 2 (Vect 6 Integer)
        addM (MkHailstone px0 py0 pz0 vx0 vy0 vz0) (MkHailstone pxn pyn pzn vxn vyn vzn) =
            [ [vy0 - vyn, vxn - vx0, 0, pyn - py0, px0 - pxn, 0]
            , [vz0 - vzn, 0, vxn - vx0, pzn - pz0, 0, px0 - pxn]]
        
        addB : Hailstone -> Hailstone -> Vect 2 Integer
        addB (MkHailstone px0 py0 pz0 vx0 vy0 vz0) (MkHailstone pxn pyn pzn vxn vyn vzn) =
            [ px0 * vy0 - py0 * vx0 - pxn * vyn + pyn * vxn
            , px0 * vz0 - pz0 * vx0 - pxn * vzn + pzn * vxn
            ]


run : String -> Integer -> Integer -> IO ()
run filename low high =
    do
        printLn filename
        content <- Util.loadFile filename
        let hailstones := parseData $ lines content
        putStrLn $ "Part One result: " ++ (show $ part1 low high hailstones)
        putStrLn $ "Part Two result: " ++ (show $ part2 hailstones)


main : IO ()
main = do
    run "aoc2023/day24/test.txt" 7 27
    run "aoc2023/day24/input.txt" 200000000000000 400000000000000
