-- Authors: Carl Malmgren, Hannes Kaulio, Hampus de Flon
-- Grupp 16

-- | The Tetris game (main module)
module Main where

import ConsoleGUI -- cabal install ansi-terminal
--import CodeWorldGUI -- cabal install codeworld-api
import Data.Maybe (isJust)
import Shapes
import Test.QuickCheck

--------------------------------------------------------------------------------

-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame =
  Game
    { startGame = startTetris,
      stepGame = stepTetris,
      drawGame = drawTetris,
      gameInfo = defaultGameInfo prop_Tetris,
      tickDelay = defaultDelay,
      gameInvariant = prop_Tetris
    }

--------------------------------------------------------------------------------

-- * The various parts of the Tetris game implementation

-- | The state of the game
data Tetris = Tetris (Vector, Shape) Shape [Shape]

-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int, Int)

-- | The size of the well
wellSize :: (Int, Int)
wellSize = (wellWidth, wellHeight)

wellWidth = 10

wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1, y1) `vAdd` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Vector, Shape) -> Shape
place (v, s) = shiftShape v s

-- ** B4

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_, fall) well _) = prop_Shape fall && shapeSize well == wellSize

-- ** B5

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls = flipShape . wall . flipShape . wall
  where
    top r = replicate (shapeWidth (S r)) (Just Black) : r
    right = map (Just Black :)
    wall = S . top . right . rows

-- ** B6

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris f w _) = addWalls $ place f `combine` w

drawTetris' :: Tetris -> Shape
drawTetris' (Tetris (v, p) w _) = addWalls x
  where
    s = shiftShape v p
    x = combine s w

-- ** B6 End

-- ** C8

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1 : supply = map (\r -> allShapes !! (floor $ r * fromIntegral (length allShapes))) rs

-- | React to input. The function returns 'Nothing' when it's game over,
-- otherwise it returns the next state.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = return (0, movePiece (-1) t)
stepTetris MoveRight t = return (0, movePiece 1 t)
stepTetris Rotate t = return (0, rotatePiece t)

-- ** B7

-- | Move the falling piece
move :: Vector -> Tetris -> Tetris
move v (Tetris (a, s) w g) = Tetris (u, s) w g
  where
    u = vAdd v a

-- ** B8

-- | Handles the Tick action, calculates a new veritical position for the active piece
tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision n = dropNewPiece t
  | otherwise = return (0, n)
  where
    n = move (0, 1) t

{-
tick' :: Tetris -> Maybe (Int, Tetris)
tick' (Tetris ((x, y), s) w rs)
  | collision $ Tetris ((x, y -1), s) w rs = dropNewPiece t --return (0, t)
  | otherwise = return (0, move (0, 1) t)
  where
    t = Tetris ((x, y), s) w rs
 -}

-- ** C1

collision :: Tetris -> Bool
collision (Tetris f@((x, y), s) w _) =
  x < 0
    || x + sW > wellWidth
    || y + sH > wellHeight
    || place f `overlaps` w
  where
    (sW, sH) = shapeSize s

tryCollide :: (Tetris -> Tetris) -> Tetris -> Tetris
tryCollide f t
  | collision n = t
  | otherwise = n
  where
    n = f t

-- ** C2

movePiece :: Int -> Tetris -> Tetris
movePiece h = tryCollide (move (h, 0))

-- ** C4

rotate :: Tetris -> Tetris
rotate (Tetris (f, s) w r) = Tetris (f, rotateShape s) w r

-- ** C5

clamp :: Int -> Int -> Int -> Int
clamp mi ma x = max mi $ min ma x

adjust :: Tetris -> Tetris
adjust (Tetris ((x, y), s) w r) = Tetris ((clamp 0 (wellWidth - shapeWidth s) x, y), s) w r

-- ** C6

rotatePiece :: Tetris -> Tetris
rotatePiece = tryCollide (adjust . rotate)

-- ** C7

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (x, s) w (r : rs))
  | overlaps (place nextPiece) newWell = Nothing
  | otherwise = Just (count, (Tetris nextPiece newWell rs))
  where
    nextPiece = (startPosition, r)
    combined = (place (x, s)) `combine` w
    (count, newWell) = clearLines combined

-- ** C9

clearLines :: Shape -> (Int, Shape)
clearLines s = (count s, shiftShapeTo $ remove s)
  where
    count = length . filter isComplete . rows
    remove = S . filter (not . isComplete) . rows
    shiftShapeTo sx = let (w, h) = shapeSize sx in shiftShape (wellWidth - w, wellHeight - h) sx

isComplete :: Row -> Bool
isComplete r = length (filter isJust r) >= wellWidth

-- ** C10
