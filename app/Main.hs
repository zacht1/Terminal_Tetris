module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List (transpose)

-- Board grid is 20x10, each cell has a size of 40

data GameState = GameState
  { 
    grid :: [[Bool]],       -- Tettris game grid represented by 2d array of Bools, True if square is occupied, False otherwise
    currPiece :: Piece,
    seed :: StdGen,
    tick :: Float           -- Time measurement for falling piece
  }

data Piece = Piece 
  { 
    shape :: [[Bool]],
    positionX :: Int,
    positionY :: Int,
    pieceDir :: Direction
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
allShapes = [shapeI, shapeJ, shapeL, shapeO, shapeS, shapeT, shapeZ]

randomPiece :: StdGen -> (Piece, StdGen)
randomPiece seed = (Piece { shape = newShape, positionX = 4, positionY = 15, pieceDir = None }, gen)
   where newShape = allShapes !! rand
         (rand, gen) = randomR (0,6) seed

initialState :: StdGen -> GameState
initialState s = GameState 
   { 
    grid = replicate 20 (replicate 10 False), 
    currPiece = piece,
    seed = gen,
    tick = 0
   }
   where (piece, gen) = randomPiece s

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
main = do
   seed <- getStdGen
   play
      (InWindow "Tetris" (400, 800) (0, 0))
      white
      60
      (initialState seed)
      render
      handleInput
      update

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state  = movePiece MoveSouth state -- state { currPiece = (currPiece state) { pieceDir = MoveSouth } }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state    = movePiece None state -- state { currPiece = (currPiece state) { pieceDir = None } }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state  = movePiece MoveWest state -- state { currPiece = (currPiece state) { pieceDir = MoveWest } }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) state    = movePiece None state -- state { currPiece = (currPiece state) { pieceDir = None } }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = movePiece MoveEast state -- state { currPiece = (currPiece state) { pieceDir = MoveEast } }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) state   = movePiece None state -- state { currPiece = (currPiece state) { pieceDir = None } }
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state    = rotatePiece state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = dropPiece state
handleInput _ state                                         = state

rotatePiece :: GameState -> GameState
rotatePiece state = state { currPiece = (currPiece state) { shape = (transpose . reverse) (shape (currPiece state)) } }

dropPiece :: GameState -> GameState
dropPiece state = generateNewPiece state { currPiece = (currPiece state) { positionY = 0 } }

validMove :: Direction -> Piece -> [[Bool]] -> Int -> Int -> Bool
validMove dir piece grid x y = 
all (\(newX, newY) -> withinBounds (newX, newY) && not (occupied grid (newX, newY))) $ occupiedCells (movePiece dir piece (x, y))

--  takes two integer arguments representing the x and y coordinates to be checked and returns a boolean value
withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

newPosition :: Direction -> GameState -> (Int, Int)
newPosition dir state = 
   let x = positionX (currPiece state)
       y = positionY (currPiece state)
   in case dir of
      MoveEast  -> (x + 1, y)
      MoveWest  -> (x - 1, y)
      MoveSouth -> (x, y - 1)
      None      -> (x, y)

{- updated to pass the current state to validMove & only update the positionX and positionY of the current 
piece in currPiece if the move is valid according to the updated validMove function.-}
movePiece :: Direction -> GameState -> GameState
movePiece dir state =
  let (newX, newY) = newPosition dir state
  in if validMove dir (currPiece state) (grid state) newX newY
     then state { currPiece = (currPiece state) { positionX = newX, positionY = newY } }
     else state


occupiedCells :: Piece -> [(Int, Int)]
occupiedCells piece = [(x + (positionX piece), y + (positionY piece)) | 
                       (y, row) <- zip [len,(len-1)..] (shape piece), 
                       (x, occupado) <- zip [0..] row, occupado]
   where len = length (shape piece) - 1

generateNewPiece :: GameState -> GameState
-- set values in grid where currPiece is to True
-- choose next piece to appear and set currPiece to that
generateNewPiece state = state { grid = newGrid, currPiece = newPiece, seed = newSeed}
   where
      newGrid = foldl (\b (x, y) -> replace2D x y True b) (grid state) (occupiedCells (currPiece state))
      (newPiece, newSeed) = randomPiece (seed state)

-- Function to replace an element at a specific position in a 2D list
replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D x y newElem xs =
    take y xs ++ [take x (xs !! y) ++ [newElem] ++ drop (x + 1) (xs !! y)] ++ drop (y + 1) xs

{- updated to pass the current state (including currPiece and grid) to the validMove function when checking for a valid south move. -}
update :: Float -> GameState -> GameState
update dt state =
  if validMove MoveSouth (currPiece state) (grid state) (positionX (currPiece state)) (positionY (currPiece state) - 1)
    then if (tick state) >= 1.0
         then movePiece MoveSouth state { tick = 0 }
         else state { tick = (tick state) + dt }
    else if (tick state) >= 1.0 then generateNewPiece state { tick = 0 } else state { tick = (tick state) + dt }


