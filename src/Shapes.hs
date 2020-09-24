-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where

import Data.List (transpose)
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
data Shape = S [Row] deriving (Eq)

type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey) = 'g' -- can change to '▓'
    showSquare (Just c) = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
  where
    makeSquares = map (map colour)
    colour c =
      lookup
        c
        [ ('I', Red),
          ('J', Grey),
          ('T', Blue),
          ('O', Yellow),
          ('Z', Cyan),
          ('L', Green),
          ('S', Purple)
        ]
    shapes =
      [ [ "I",
          "I",
          "I",
          "I"
        ],
        [ " J",
          " J",
          "JJ"
        ],
        [ " T",
          "TT",
          " T"
        ],
        [ "OO",
          "OO"
        ],
        [ " Z",
          "ZZ",
          "Z "
        ],
        [ "LL",
          " L",
          " L"
        ],
        [ "S ",
          "SS",
          " S"
        ]
      ]

-- * Some simple functions

-- ** A1

-- returns empty row of a given size
emptyRow :: Int -> Row
emptyRow w = replicate w Nothing

-- returns empty shape of a given size
emptyShape :: (Int, Int) -> Shape
emptyShape (w, h) = S $ replicate h $ emptyRow w

-- ** A2

-- Get width of shape
shapeWidth :: Shape -> Int
shapeWidth = length . head . rows

-- Get height of shape
shapeHeigth :: Shape -> Int
shapeHeigth = length . rows

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S []) = (0, 0)
-- shapeSize (S row) = (length $ head row, length row)
shapeSize s = (shapeWidth s, shapeHeigth s)

-- | Test that shapeSize gives correct size
prop_ShapeSize :: Int -> Int -> Bool
prop_ShapeSize w h = shapeSize (emptyShape (w, h)) == (w, h)

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S row) = length $ filter isJust $ concat row

{- Alternative
blockCount' :: Shape -> Int
blockCount' (S row) = length [n | n <- r, n /= Nothing]
  where
    r = concat row
 -}

-- * The Shape invariant

-- ** A4

-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S row) = w >= 1 && h >= 1 && all len row
  where
    (w, h) = shapeSize (S row)
    len x = length x == w

-- * Test data generators

-- ** A5

-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6

-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7

-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape --rotates counterclockwise
rotateShape (S s) = S $ reverse $ transpose s

{- Alternative
rotateShape' :: Shape -> Shape --rotates clockwise
rotateShape' = S . transpose . reverse . rows
-}

-- ** A8

-- Adds empty lines to the left of the shape
shiftRight :: Int -> Shape -> Shape
shiftRight 0 s = s
shiftRight i s = S $ map (Nothing :) $ rows $ shiftRight (i -1) s

-- Adds empty lines above the shape
shiftDown :: Int -> Shape -> Shape
shiftDown i (S s) = S $ replicate i (emptyRow (shapeWidth (S s))) ++ s

{- Alternative
shiftDown' :: Int -> Shape -> Shape
shiftDown' 0 s = s
shiftDown' i s = S $ (emptyRow $ shapeWidth s) : (rows $ shiftDown' (i -1) s)
-}

-- | Adds empty lines above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (x, y) s = shiftDown y (shiftRight x s)

-- ** A9

-- | Rotate shape 180 degrees
flipShape :: Shape -> Shape
flipShape = S . reverse . map reverse . rows

-- | Add empty lines below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (x, y) = flipShape . shiftShape (x, y) . flipShape

-- Alternative

shiftUp :: Int -> Shape -> Shape
shiftUp i s = S $ (rows s) ++ (take i (rows $ shiftDown i s))

shiftLeft :: Int -> Shape -> Shape
shiftLeft 0 s = s
shiftLeft i s = S $ map (++ [Nothing]) $ rows $ shiftLeft (i -1) s

padShape' :: (Int, Int) -> Shape -> Shape
padShape' (x, y) s = shiftUp y (shiftLeft x s)

-- ** A10

-- | Pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (x, y) s = padShape (max (x - w) 0, max (y - h) 0) s
  where
    (w, h) = shapeSize s

{-  Alternative
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (x, y) s
  | x <= shapex && y <= shapey = s
  | x <= shapex = padShape (0, y - shapey) s
  | y <= shapey = padShape (x - shapex, 0) s
  | otherwise = padShape (x - shapex, y - shapey) s
  where
    (shapex, shapey) = shapeSize s
-}

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2

-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3

-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
