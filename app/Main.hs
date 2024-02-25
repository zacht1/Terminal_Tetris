module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Board grid is 20x10, each cell has a size of 40

data GameState = GameState
  { 
    grid :: [[Bool]],       -- Tettris game grid represented by 2d array of Bools, True if square is occupied, False otherwise
    currPiece :: Piece,
    pieceDir :: Direction
  }

data Piece = Piece 
  { 
    shape :: [[Bool]],
    positionX :: Int,
    positionY :: Int
  }

data Direction = MoveWest | MoveEast | MoveSouth | None

-- Possible shapes
shapeI, shapeJ, shapeL, shapeO, shapeS, shapeT, shapeZ :: [[Bool]]
shapeI = [[True], [True], [True], [True]]
shapeJ = [[False, True], [False, True], [True, True]]
shapeL = [[True, False], [True, False], [True, True]]
shapeO = [[True, True], [True, True]]
shapeS = [[True, False], [True, True], [False, True]]
shapeT = [[False, True], [True, True], [False, True]]
shapeZ = [[False, True], [True, True], [True, False]]

initialState:: GameState
initialState = GameState 
   { 
    grid = replicate 20 (replicate 10 False), 
    currPiece = Piece {shape = shapeZ, positionX = 4, positionY = 15},
    pieceDir = None
   }

renderCell :: (Int, Int) -> Color -> Picture
renderCell (x, y) c = translate (fromIntegral (x * 40 - 180)) (fromIntegral (y * 40 - 380))
                      $ color c
                      $ rectangleSolid 40 40

render :: GameState -> Picture
-- render state = translate x y $ color blue $ shapeJ 20
--   where
--     (x, y) = piecePos state
render state = pictures $ 
   [renderCell (x, y) blue | (y, row) <- zip [0..] (grid state), (x, occupado) <- zip [0..] row, occupado] ++
   [renderCell (x + (positionX (currPiece state)), y + (positionY (currPiece state))) blue | 
   (y, row) <- zip [0..] (shape (currPiece state)), (x, occupado) <- zip [0..] row, occupado]


main :: IO ()
main = play
  (InWindow "Tetris" (420, 820) (0, 0))
  white
  60
  initialState
  render
  handleInput
  update

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = state -- { pieceDir = MoveSouth }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state    = state -- { pieceDir = None }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = state -- { pieceDir = MoveWest }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) state    = state -- { pieceDir = None }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state -- { pieceDir = MoveEast }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) state   = state -- { pieceDir = None }
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = state -- { piecePos = dropPiece state }
handleInput _ state                                         = state

-- dropPiece :: GameState -> Point
-- dropPiece state = (x, -340)
--    where (x, _) = piecePos state

update :: Float -> GameState -> GameState
update dt state = state
-- update dt state = 
--   let (x, y) = piecePos state
--   in case pieceDir state of
--      MoveEast  -> state { piecePos = (x + 10 * dt * 6, y) }
--      MoveWest  -> state { piecePos = (x - 10 * dt * 6, y) }
--      MoveSouth -> state { piecePos = (x, y - 10 * dt * 6) }
--      None  -> state