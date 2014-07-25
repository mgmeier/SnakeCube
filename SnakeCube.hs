{-# OPTIONS_GHC                 -funbox-strict-fields #-}


import  System.IO.Unsafe        (unsafePerformIO)                       -- for debug/progress output only

import  qualified Data.HashSet  as HS                                   -- Data.Set seems to perform worse here, almost doubling execution time
import  Data.Hashable           (Hashable, hashWithSalt)




-- some data representing the cubelet count of the segments of a snake cube.
-- NB. any cubelet must be counted only once in total. that's also why
-- there are segments of length 1: their actual langth is two, but one
-- cubelet already counted for the segment beforehand. so if you want
-- to write down the segments' absolute length, you should
-- map (subtract 1)
-- to the tail of your list.


-- snake cube with edge length 2 - the hardest one !1!!
cubeLength2     = 2
cubeSegments2   = [2, 1, 1, 1, 1, 1, 1]


-- snake cube with edge length 3
-- (compare with image in README.md to see how the segments were encoded)
cubeLength3         = 3
cubeSegments3LtoR   = [3, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2]
cubeSegmentsR3toL   = [3, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2]


-- snake cube with edge length 4
cubeLength4     = 4
cubeSegments4   =                                                       -- TODO: can't currently be solved... see README.md for details
    [ 4, 1, 3, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1
    , 2, 1, 3, 1, 2, 2, 3, 1, 2, 1, 1, 1, 1
    , 1, 1, 1, 1, 1, 3, 1, 3, 1, 3, 3, 3, 2
    ]



-- a three-dimensional vector
data Move   = Move  !Int !Int !Int


-- the same as above, but used in a different context
data Coord  = Coord !Int !Int !Int  deriving Eq


-- a path is a set of coordinates which occupy it,
-- the coordinate from which the path can be extended, and a list
-- of Moves leading to that coordinate.
-- NB. the moves are in reverse order, meaning the "opening" Move
-- (the one starting from (0,0,0)) is last in list.
data Path  = Path  !(HS.HashSet Coord) !Coord [Move]



instance Hashable Coord where
    hashWithSalt salt (Coord x y z) =
        salt    `hashWithSalt` x
                `hashWithSalt` y
                `hashWithSalt` z


instance Show Coord where                                               -- for debug output, currently not needed
    show (Coord x y z) =
        concat ["(", show x, ",", show y, ",", show z, ")"]


instance Show Path where                                                -- for debug output, currently not needed
    show (Path cs c _) = 
        "path ending in " ++ show c ++
        " occupies " ++ show cs


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



-- lukeSegmentWalker left folds over the list of segment lengths.
-- keeps track of valid paths to arrange the segments,
-- throwing away crossing paths or the ones leaving the cube area.
lukeSegmentWalker :: Int -> [Path] -> Int -> [Path]
lukeSegmentWalker edgeLen paths segLen =
    {- progressOutput `seq` -} concatMap forkPaths paths                -- UNCOMMENT FOR STATUS UPDATES DURING CALCULATION
  
  where
    forkPaths (Path coords c path@(m:_)) =
        [Path (coords' `HS.union` coords) (last newC) (move:path)
            | move <- moveChoices m segLen
            , let newC = applyMove c move
            , all isInsideCube newC                                     -- don't leave cube area
            , let coords' = HS.fromList newC
            , HS.null (coords' `HS.intersection` coords)                -- don't overlap with occupied coordinates
            ]

    isInsideCube (Coord x y z) =
        isInsideCube' x && isInsideCube' y && isInsideCube' z
      where
        {-# INLINE isInsideCube' #-}
        isInsideCube' c = c > 0 && c <= edgeLen

    progressOutput = unsafePerformIO $ putStrLn $
        "berechne Segment: " ++ show segLen
        ++ "   belegte Koordinaten: " ++ show occupiedCoos
        ++ "   aktuell mögliche Pfade: " ++ show (length paths)
      where
        occupiedCoos = case paths of
            []              -> 0
            (Path cs _ _):_ -> HS.size cs



-- this function produces epic win!
solveSnakeCube :: Int -> [Int] -> IO ()
solveSnakeCube edgeLen segments@(seg:segs) = do
    putStrLn $
        "Würfel mit Kantenlänge " ++ show edgeLen
        ++ " und den Segmenten:\n" ++ show (seg:map (+1) segs)
        ++ "\nBerechne... "
        ++ "(kann bei einem 4x4x4 Würfel auch mal über 1 min dauern)"

    let 
        insaneCube
            | any (\i -> i < 1 || i > edgeLen) segments =
                "Ein Segment hat hier wohl die falsche Länge!"          -- ill-sized segments found
            | sum segments /= edgeLen^3 =
                "Segmentlängen ergeben keinen Würfel!"                  -- wrong length of snake
            | otherwise =
                ""                                                      -- ok!

        startMove = Move 0 0 seg
        startCoos = applyMove (Coord 1 1 0) startMove
        startPath = Path (HS.fromList startCoos) (last startCoos) [startMove]       
        goodPaths = foldl (lukeSegmentWalker edgeLen) [startPath] segs

    putStrLn $ if null insaneCube
        then case goodPaths of
            []      ->                                                  -- no solution for given segments
                "Diese Segmente lassen sich nicht \
                \ zu einem Würfel falten."
            sol:_   ->                                                  -- there will always be min. 2 solutions because of symmetry, we discard all but one
                intro ++ (prettyPrint . movesfromPath) sol
        else insaneCube

    where
        intro =
            "\nNimm die Würfelschlange von dem Ende her, so dass sie \
            \der obigen Liste entspricht.\nDas erste Segment hat die \
            \Länge " ++ show seg ++ ". Leg es so vor dich \
            \auf den Tisch,\ndass es von dir weg, \"nach hinten\", \
            \zeigt. Dann drehst du die Segmente\naus *deiner* \
            \Sicht einfach der Reihe nach wie folgt angegeben:\n"

        movesfromPath (Path _ _ ms) =
            reverse ms

        prettyPrint (_:moves) =                                         -- first Move already described in intro
            let indexed = zip [1..] moves
            in unlines $ 
                map (\(i, m) -> show i ++ ". --> " ++ show m) indexed 



--
--  MAIN
--

main :: IO ()
main =                                                                  -- UNCOMMENT WHICHEVER TASK YOU'D LIKE
    -- solveSnakeCube cubeLength2 cubeSegments2
    solveSnakeCube cubeLength3 cubeSegments3LtoR
    -- solveSnakeCube cubeLength3 cubeSegmentsR3toL
    -- solveSnakeCube cubeLength4 cubeSegments4
