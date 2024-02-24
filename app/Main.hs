module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GameState = GameState
  { currPiece :: Piece
    piecePos :: Point,
    pieceDir :: Direction
  }

data Piece = I | J | L | O | S | Z | T
data Direction = MoveWest | MoveEast | MoveSouth | None

initialState:: GameState
initialState = GameState { piecePos = (0, 0), pieceDir = None }

main :: IO ()
main = play
  (InWindow "Tetris" (400, 700) (10, 10))
  white
  60
  initialState
  draw
  handleInput
  update

draw :: GameState -> Picture
draw state = translate x y $ color blue $ rectangleSolid 20 20
  where
    (x, y) = piecePos state

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = state { pieceDir = MoveSouth }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state    = state { pieceDir = None }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = state { pieceDir = MoveWest }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) state  = state { pieceDir = None }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = state { pieceDir = MoveEast }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) state  = state { pieceDir = None }
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = state { piecePos = dropPiece state }
handleInput _ state                                         = state

dropPiece :: GameState -> Point
dropPiece state = (x, -340)
   where (x, _) = piecePos state

update :: Float -> GameState -> GameState
update dt state = 
  let (x, y) = piecePos state
  in case pieceDir state of
     MoveEast  -> state { piecePos = (x + 10 * dt * 6, y) }
     MoveWest  -> state { piecePos = (x - 10 * dt * 6, y) }
     MoveSouth -> state { piecePos = (x, y - 10 * dt * 6) }
     None  -> state