module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List (transpose)

-- Board grid is 20x10, each cell has a size of 40
-- Constants
width, height, cellSize :: Int
width = 10
height = 20
cellSize = 40

data GameState = GameState
  { 
    grid :: [[Bool]],       -- Tettris game grid represented by 2d array of Bools, True if square is occupied, False otherwise
    currPiece :: Piece,
    seed :: StdGen,
    tick :: Float,          -- Time measurement for falling piece
    gameOver :: Bool        -- True if the game has ended
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
allShapes :: [[[Bool]]]
allShapes = [shapeI, shapeJ, shapeL, shapeO, shapeS, shapeT, shapeZ]

randomPiece :: StdGen -> (Piece, StdGen)
randomPiece s = (Piece { shape = newShape, positionX = 4, positionY = 15, pieceDir = None }, gen)
   where newShape = allShapes !! rand
         (rand, gen) = randomR (0,6) s

initialState :: StdGen -> GameState
initialState s = GameState 
   { 
    grid = replicate height (replicate width False), 
    currPiece = piece,
    seed = gen,
    tick = 0,
    gameOver = False
   }
   where (piece, gen) = randomPiece s

renderCell :: (Int, Int) -> Color -> Picture
renderCell (x, y) c = translate (fromIntegral (x * cellSize - 180)) (fromIntegral (y * cellSize - 380))
                      $ color c
                      $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)

render :: GameState -> Picture
-- render state = translate x y $ color blue $ shapeJ 20
--   where
--     (x, y) = piecePos state
render state = 
   if (gameOver state) 
   then pictures $
      [translate (-100) 0 $ scale 0.3 0.3 $ color red $ text "Game Over"] ++ 
      [translate (-65) (-25) $ scale 0.1 0.1 $ color red $ text "Press ' R ' to Restart"]
   else pictures $
      [renderCell (x, y) red | (y, row) <- zip [0..] (grid state), (x, occupado) <- zip [0..] row, occupado] ++
      [renderCell (x + (positionX (currPiece state)), y + (positionY (currPiece state))) blue | 
      (y, row) <- zip [0..] (shape (currPiece state)), (x, occupado) <- zip [0..] row, occupado]

main :: IO ()
main = do
   s <- getStdGen
   play
      (InWindow "Tetris" (400, 800) (0, 0))
      white
      60
      (initialState s)
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
handleInput (EventKey (Char 'r') Down _ _) state            = if (gameOver state) then initialState (seed state) else state
handleInput _ state                                         = state

-- TODO: rotate piece does not check that the rotation would cause the piece to move illegally
rotatePiece :: GameState -> GameState
rotatePiece state = state { currPiece = (currPiece state) { shape = (transpose . reverse) (shape (currPiece state)) } }

-- TODO: drop piece does not currently account for pieces already placed
dropPiece :: GameState -> GameState
dropPiece state = generateNewPiece state { currPiece = (currPiece state) { positionY = 0 } }
   -- where lastY = findIndex (\_ -> validMove MoveSouth piece state)
   --       numCells = [py, py-1..0] -- (positionY piece)
   --       py = (positionY piece) - 1
   --       piece = (currPiece state)

--  takes two integer arguments representing the x and y coordinates to be checked and returns a boolean value
withinBounds :: (Int, Int) -> Bool
withinBounds (x, y) = x < width && x >= 0 && y >= 0

unoccupied :: (Int, Int) -> [[Bool]] -> Bool
unoccupied (x, y) g = not $ (g !! y) !! x

validMove :: Direction -> Piece -> GameState -> Bool
validMove dir piece state = all (\pt -> withinBounds pt && unoccupied pt (grid state)) pieceCells
   where pieceCells = [(x + pieceX, y + pieceY) | (y, row) <- zip [0..] (shape piece),
                               (x, occupado) <- zip [0..] row,
                               occupado]
         (pieceX, pieceY) = newPosition dir state

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
  in if validMove dir (currPiece state) state -- (grid state) newX newY
     then state { currPiece = (currPiece state) { positionX = newX, positionY = newY } }
     else state

occupiedCells :: Piece -> [(Int, Int)]
-- Cells occupied by current piece
occupiedCells piece = [(x + (positionX piece), y + (positionY piece)) | 
                       (y, row) <- zip [0..] (shape piece), 
                       (x, occupado) <- zip [0..] row, occupado]

generateNewPiece :: GameState -> GameState
-- set values in grid where currPiece is to True
-- choose next piece to appear and set currPiece to that
generateNewPiece state = state { grid = newGrid, currPiece = newPiece, seed = newSeed, gameOver = isOver}
   where
      isOver = any (\x -> x) (newGrid !! 15)
      newGrid = foldl (\b (x, y) -> replace2D x y True b) (grid state) (occupiedCells (currPiece state))
      (newPiece, newSeed) = randomPiece (seed state)

-- Function to replace an element at a specific position in a 2D list
replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D x y newElem xs =
    take y xs ++ [take x (xs !! y) ++ [newElem] ++ drop (x + 1) (xs !! y)] ++ drop (y + 1) xs

-- TODO : clear rows when completed

{- updated to pass the current state (including currPiece and grid) to the validMove function when checking for a valid south move. -}
update :: Float -> GameState -> GameState
update dt state =
   if validMove MoveSouth (currPiece state) state -- (grid state) (positionX (currPiece state)) (positionY (currPiece state) - 1)
   then if (tick state) >= 1.0
        then movePiece MoveSouth state { tick = 0 }
        else state { tick = (tick state) + dt }
   else if (tick state) >= 1.0 then generateNewPiece state { tick = 0 } else state { tick = (tick state) + dt }


