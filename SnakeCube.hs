{-# OPTIONS_GHC                         -funbox-strict-fields #-}


import  Control.Parallel.Strategies
import  Control.Monad                   (msum)

import  qualified Data.HashSet          as HS                           -- Data.Set tested; seemed to perform worse here
import  Data.Hashable                   (Hashable, hashWithSalt)



-- some data representing the cubelet count of the segments of a snake cube.
-- compare cubeSegments3 with image in README.md to see how the
-- segments were encoded, left to right.


-- snake cube with edge length 2 - the hardest one !1!!
cubeLength2     = 2
cubeSegments2   = [2, 2, 2, 2, 2, 2, 2]


-- snake cube with edge length 3
cubeLength3     = 3
cubeSegments3   = [3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3]


-- snake cube with edge length 4
cubeLength4     = 4
cubeSegments4   =
    [ 3, 2, 3, 2, 2, 4, 2, 3, 2, 3, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 3
    , 3, 2, 2, 2, 2, 2, 3, 4, 2, 2, 2, 4, 2, 3, 2, 2, 2, 2, 2, 2, 2
    , 2, 2, 4, 2]




-- a three-dimensional vector
data Move   = Move  !Int !Int !Int


-- the same as above, but used in a different context
data Coord  = Coord !Int !Int !Int  deriving Eq


-- a path is a set of coordinates which occupy it,
-- the coordinate from which the path can be extended, and a list
-- of Moves leading to that coordinate.
-- NB. the moves are in reverse order, meaning the "opening" Move
-- (the one starting from (0,0,0)) is last in list.
data Path  = Path  (HS.HashSet Coord) Coord [Move]



instance Hashable Coord where
    hashWithSalt salt (Coord x y z) =
        salt    `hashWithSalt` x
                `hashWithSalt` y
                `hashWithSalt` z


-- a custom Show instance which directly translates a Move vector
-- into a natural language instruction on how to arrange or proceed
-- arranging the cube's segments; always from the user's pov.
instance Show Move where
    show (Move 0 0 a)   = if a < 0 then "nach vorn"  else "nach hinten"
    show (Move 0 a 0)   = if a < 0 then "nach unten" else "nach oben"
    show (Move a 0 0)   = if a < 0 then "nach links" else "nach rechts"
    show _              = "... irgendwas stimmt hier nicht!"




-- depending upon the last applied Move and the length of the
-- current segment, a list of follow-up Move vectors is generated.
-- e.g. if the last Move was along the y-axis, the following one
-- had to be along the x- or z-axis.
moveChoices :: Move -> Int -> [Move]
moveChoices (Move x y z) seg
    | x /= 0    = yAxis ++ zAxis
    | y /= 0    = xAxis ++ zAxis
    | z /= 0    = xAxis ++ yAxis
    | otherwise = error "moveChoices: fehlerhafter Move"

  where
    xAxis   = [Move seg 0 0, Move (-seg) 0 0]
    yAxis   = [Move 0 seg 0, Move 0 (-seg) 0]
    zAxis   = [Move 0 0 seg, Move 0 0 (-seg)]



-- generates a list of coordinates occupied by some move
applyMove :: Coord -> Move -> [Coord]
applyMove c (Move x y z) = 
    let
        steps       = abs (x+y+z)
        moveStep    = Move (normalize x) (normalize y) (normalize z)
    in tail . take (steps+1) . iterate (step moveStep) $ c
  
  where
    step (Move dx dy dz) (Coord x' y' z') =
        Coord (dx+x') (dy+ y') (dz+z')

    {-# INLINE normalize #-}
    normalize x
        | x == 0    = 0
        | x > 0     = 1
        | otherwise = -1



-- each recursive step tries to extend the path with a given segemnt,
-- throwing away crossing paths or the ones leaving the cube area.
-- when a path can't be extended further, the recursion branch is pruned.
solveRecursive :: Int -> [Int] -> Path -> Maybe Path
solveRecursive edgeLen =
    recurse
  where
    recurse [] path = Just path                                         -- no segments left? found solution
    recurse (s:segs) (Path coords c path@(m:_)) =
        msum . (Nothing:) . parMap rpar (recurse segs) $
            [Path (HS.fromList newC `HS.union` coords) (last newC) (move:path)
                | move <- moveChoices m s
                , let newC = applyMove c move
                , all isInsideCube newC                                 -- don't leave cube area
                , all (not . flip HS.member coords) newC                -- don't overlap with occupied coordinates
                ]

    isInsideCube (Coord x y z) =
        isInsideCube' x && isInsideCube' y && isInsideCube' z
      where
        {-# INLINE isInsideCube' #-}
        isInsideCube' c = c > 0 && c <= edgeLen



-- this function produces epic win!
solveSnakeCube :: Int -> [Int] -> IO ()
solveSnakeCube edgeLen seg_ = do
    putStrLn $
        "Würfel mit Kantenlänge " ++ show edgeLen
        ++ " und den Segmenten:\n" ++ show seg_
        ++ "\nBerechne... "
        ++ "(kann bei einem 4x4x4 Würfel auch u.U. 1 min dauern)"

    let 
        insaneCube
            | any (\i -> i < 1 || i > edgeLen) seg_ =
                "Ein Segment hat hier wohl die falsche Länge!"          -- ill-sized segments found
            | sum segments /= edgeLen^3 =
                "Segmentlängen ergeben keinen Würfel!"                  -- wrong length of snake
            | otherwise =
                ""                                                      -- ok!

        startMove   = Move 0 0 seg
        startCoos   = applyMove (Coord 1 1 0) startMove
        startPath   = Path (HS.fromList startCoos) (last startCoos) [startMove]       
        solution    = solveRecursive edgeLen segs startPath

    putStrLn $ if null insaneCube
        then maybe noSolution ((intro ++) . prettyPrintMoves) solution
        else insaneCube

    where
        segments@(seg:segs) =
            head seg_ : map (subtract 1) (tail seg_)                    -- to avoid double-counting cubelets

        noSolution =
            "Diese Segmente lassen sich nicht zu einem Würfel falten."

        intro =
            "\nNimm die Würfelschlange von dem Ende her, so dass sie \
            \der obigen Liste entspricht.\nDas erste Segment hat die \
            \Länge " ++ show seg ++ ". Leg es so vor dich \
            \auf den Tisch,\ndass es von dir weg, \"nach hinten\", \
            \zeigt. Dann drehst du die Segmente\naus *deiner* \
            \Sicht einfach der Reihe nach wie folgt angegeben:\n"

        prettyPrintMoves (Path _ _ ms) =                                         
            let indexed = zip [1..] (tail . reverse $ ms)               -- first Move already described in intro
            in unlines $ 
                map (\(i, m) -> show i ++ ". --> " ++ show m) indexed 



main :: IO ()
main =                                                                  -- UNCOMMENT WHICHEVER TASK YOU'D LIKE
    -- solveSnakeCube cubeLength2 cubeSegments2
    -- solveSnakeCube cubeLength3 cubeSegments3
    solveSnakeCube cubeLength4 cubeSegments4
